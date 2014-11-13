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

import qualified Data.ByteString.Char8 as BS ( ByteString, pack, unpack, empty, null)
import qualified Data.ByteString.Lazy as LBS ( ByteString, length )
import Data.Monoid ( mconcat )
import qualified Network.Socket as NS
import Network.Socket.Options ( setRecvTimeout, setSendTimeout )
import Network.Socket.ByteString.Lazy ( sendAll )
import Control.Monad ( void, forever, when )
import Control.Applicative ( (<$>) )
import Control.Concurrent ( ThreadId, forkIO, killThread, threadDelay )
import Control.Concurrent.STM ( atomically
                              , TChan, newTChanIO, readTChan, peekTChan, writeTChan
                              , TVar, newTVarIO, readTVar, modifyTVar )
import Control.Exception ( SomeException, handle, bracket, throwIO )
import Data.MessagePack
import Data.Serialize hiding (label)
import Data.Int ( Int64 )
import Data.Time.Clock.POSIX ( getPOSIXTime )
import System.Random ( randomRIO )

import Network.Fluent.Logger.Internal

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
    , fluentSettingsHost = BS.pack "localhost"
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
    { fluentLoggerSender :: FluentLoggerSender
    , fluentLoggerThread :: ThreadId
    }

data FluentLoggerSender =
    FluentLoggerSender
    { fluentLoggerSenderChan :: TChan LBS.ByteString
    , fluentLoggerSenderBuffered :: TVar Int64
    , fluentLoggerSenderSettings :: FluentSettings
    }

getSocket :: BS.ByteString -> Int -> Int64 -> IO NS.Socket
getSocket host port timeout = do
  let hints = NS.defaultHints { NS.addrFlags = [NS.AI_ADDRCONFIG]
                              , NS.addrSocketType = NS.Stream
                              }
  (addr:_) <- NS.getAddrInfo (Just hints) (Just $ BS.unpack host) (Just $ show port)
  sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
  setRecvTimeout sock timeout
  setSendTimeout sock timeout
  let onErr :: SomeException -> IO a
      onErr e = NS.sClose sock >> throwIO e
  handle onErr $ do
    NS.connect sock $ NS.addrAddress addr
    return sock

runSender :: FluentLoggerSender -> IO ()
runSender logger = forever $ connectFluent logger >>= sendFluent logger

connectFluent :: FluentLoggerSender -> IO NS.Socket
connectFluent logger = exponentialBackoff $ getSocket host port timeout where
    set = fluentLoggerSenderSettings logger
    host = fluentSettingsHost set
    port = fluentSettingsPort set
    timeout = round $ fluentSettingsTimeout set * 1000000

exponentialBackoff :: IO a -> IO a
exponentialBackoff action = handle (retry 100000) action where
    retry failCount exception =
        let _ = exception :: SomeException
        in exponentialBackoff' failCount
    exponentialBackoff' interval = do
      delay <- randomRIO (interval `div` 2, interval * 3 `div` 2)
      threadDelay delay
      handle (retry $ min 60000000 $ interval * 3 `div` 2) action

sendFluent :: FluentLoggerSender -> NS.Socket -> IO ()
sendFluent logger sock = handle (done sock) toSender where
    chan = fluentLoggerSenderChan logger
    buffered = fluentLoggerSenderBuffered logger
    done :: NS.Socket -> SomeException -> IO ()
    done = const . NS.sClose
    toSender = do
      entry <- atomically $ peekTChan chan
      sendAll sock entry
      atomically $ do
        void $ readTChan chan
        modifyTVar buffered (subtract $ LBS.length entry)
      sendFluent logger sock

-- | Create a fluent logger
--
-- Since 0.1.0.0
--
newFluentLogger :: FluentSettings -> IO FluentLogger
newFluentLogger set = do
  tchan <- newTChanIO
  tvar <- newTVarIO 0
  let sender = FluentLoggerSender
               { fluentLoggerSenderChan = tchan
               , fluentLoggerSenderBuffered = tvar
               , fluentLoggerSenderSettings = set
               }
  tid <- forkIO $ runSender sender
  let logger = FluentLogger
               { fluentLoggerSender = sender
               , fluentLoggerThread = tid
               }
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
postWithTime :: (Packable a) => FluentLogger -> BS.ByteString -> Int -> a -> IO ()
postWithTime logger label time obj = atomically send where
    sender = fluentLoggerSender logger
    set = fluentLoggerSenderSettings sender
    tag = fluentSettingsTag set
    lbl = if BS.null label then tag else mconcat [ tag, BS.pack ".", label ]
    entry = encodeLazy $ ObjectArray [ObjectBinary lbl, ObjectInt (fromIntegral time), pack obj]
    len = LBS.length entry
    chan = fluentLoggerSenderChan sender
    buffered = fluentLoggerSenderBuffered sender
    limit = fluentSettingsBufferLimit set
    send = do
      s <- readTVar buffered
      when (s + len <= limit) $ do
        writeTChan chan entry
        modifyTVar buffered (+ len)
