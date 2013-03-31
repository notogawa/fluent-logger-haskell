--
-- Copyright (C) 2012 Noriyuki OHKAWA
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
-- | Fluent Logger for Haskell
module Network.Fluent.Logger
    ( -- * Logger
      FluentLogger
    , withFluentLogger
    , newFluentLogger
    , closeFluentLogger
      -- * Settings
    , FluentSettings(..)
    , defaultFluentSettings
      -- * Post
    , post
    , postWithTime
    ) where

import qualified Data.ByteString as BS
import Data.ByteString.Char8 ( unpack )
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid ( mconcat )
import qualified Network.Socket as NS
import Network.Socket.Options ( setRecvTimeout, setSendTimeout )
import Network.Socket.ByteString.Lazy ( sendAll )
import Control.Monad ( void, forever, when )
import Control.Applicative ( (<$>) )
import Control.Concurrent ( ThreadId, forkIO, killThread )
import Control.Concurrent.STM ( atomically
                              , TChan, newTChanIO, readTChan, peekTChan, writeTChan
                              , TVar, newTVarIO, readTVar, modifyTVar )
import Control.Exception ( SomeException, handle, bracket, throwIO )
import Data.MessagePack ( Packable, pack )
import Data.Int ( Int64 )
import Data.Time.Clock.POSIX ( getPOSIXTime )

-- | Fluent logger settings
--
-- Since 0.1.0.0
--
data FluentSettings =
    FluentSettings
    { fluentSettingsTag :: BS.ByteString
    , fluentSettingsHost :: BS.ByteString
    , fluentSettingsPort :: Int
    , fluentSettingsTimeout :: Double
    , fluentSettingsBufferLimit :: Int64
    }

-- | Default fluent logger settings
--
-- Since 0.1.0.0
--
defaultFluentSettings :: FluentSettings
defaultFluentSettings =
    FluentSettings
    { fluentSettingsTag = BS.empty
    , fluentSettingsHost = "localhost"
    , fluentSettingsPort = 24224
    , fluentSettingsTimeout = 3.0
    , fluentSettingsBufferLimit = 1024*1024
    }

-- | Fluent logger
--
-- Since 0.1.0.0
--
data FluentLogger =
    FluentLogger
    { fluentLoggerChan :: TChan LBS.ByteString
    , fluentLoggerBuffered :: TVar Int64
    , fluentLoggerSettings :: FluentSettings
    , fluentLoggerThread :: ThreadId
    }

getSocket :: BS.ByteString -> Int -> Int64 -> IO NS.Socket
getSocket host port timeout = do
  let hints = NS.defaultHints { NS.addrFlags = [NS.AI_ADDRCONFIG]
                              , NS.addrSocketType = NS.Stream
                              }
  (addr:_) <- NS.getAddrInfo (Just hints) (Just $ unpack host) (Just $ show port)
  sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
  setRecvTimeout sock timeout
  setSendTimeout sock timeout
  let onErr :: SomeException -> IO a
      onErr e = NS.sClose sock >> throwIO e
  handle onErr $ do
    NS.connect sock $ NS.addrAddress addr
    return sock

sender :: FluentLogger -> IO ()
sender logger = forever $ connectFluent logger >>= sendFluent logger

connectFluent :: FluentLogger -> IO NS.Socket
connectFluent logger = handle (retry logger) (getSocket host port timeout)
    where
      host = fluentSettingsHost $ fluentLoggerSettings logger
      port = fluentSettingsPort $ fluentLoggerSettings logger
      timeout = round $ fluentSettingsTimeout (fluentLoggerSettings logger) * 1000000
      retry :: FluentLogger -> SomeException -> IO NS.Socket
      retry = const . connectFluent

sendFluent :: FluentLogger -> NS.Socket -> IO ()
sendFluent logger sock = handle (done sock) $ do
                           entry <- atomically $ peekTChan chan
                           sendAll sock entry
                           atomically $ do
                             void $ readTChan chan
                             modifyTVar buffered (subtract $ LBS.length entry)
                           sendFluent logger sock
    where
      chan = fluentLoggerChan logger
      buffered = fluentLoggerBuffered logger
      done :: NS.Socket -> SomeException -> IO ()
      done = const . NS.sClose

-- | Create a fluent logger
--
-- Since 0.1.0.0
--
newFluentLogger :: FluentSettings -> IO FluentLogger
newFluentLogger set = do
  tchan <- newTChanIO
  tvar <- newTVarIO 0
  let mkLogger tid = FluentLogger { fluentLoggerChan = tchan
                                  , fluentLoggerBuffered = tvar
                                  , fluentLoggerSettings = set
                                  , fluentLoggerThread = tid
                                  }
  rec logger <- mkLogger <$> forkIO (sender logger)
  return logger

-- | Close logger
--
-- Since 0.1.0.0
--
closeFluentLogger :: FluentLogger -> IO ()
closeFluentLogger = killThread . fluentLoggerThread

-- | Create a fluent logger and run given action.
--
-- Since 0.1.0.0
--
withFluentLogger :: FluentSettings -> (FluentLogger -> IO a) -> IO a
withFluentLogger set = bracket (newFluentLogger set) closeFluentLogger

getCurrentEpochTime :: IO Int
getCurrentEpochTime = round <$> getPOSIXTime

-- | Post a message.
--
-- Since 0.1.0.0
--
post :: Packable a => FluentLogger -> BS.ByteString -> a -> IO ()
post logger label obj = do
  time <- getCurrentEpochTime
  postWithTime logger label time obj

-- | Post a message with given time.
--
-- Since 0.1.0.0
--
postWithTime :: Packable a => FluentLogger -> BS.ByteString -> Int -> a -> IO ()
postWithTime logger label time obj = atomically $ do
                                       s <- readTVar buffered
                                       when (s + len <= limit) $ do
                                         writeTChan chan entry
                                         modifyTVar buffered (+ len)
  where
    tag = fluentSettingsTag $ fluentLoggerSettings logger
    lbl = if BS.null label then tag else mconcat [ tag, ".", label ]
    entry = pack ( lbl, time, obj )
    len = LBS.length entry
    chan = fluentLoggerChan logger
    buffered = fluentLoggerBuffered logger
    limit = fluentSettingsBufferLimit $ fluentLoggerSettings logger
