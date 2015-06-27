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
{-# LANGUAGE CPP #-}
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
#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ( (<$>) )
import Data.Monoid ( mconcat )
#endif
import qualified Network.Socket as NS
import Network.Socket.Options ( setRecvTimeout, setSendTimeout )
import Network.Socket.ByteString.Lazy ( sendAll, recv )
import Control.Monad ( void, forever, when )
import Control.Concurrent ( ThreadId, killThread, threadDelay )
import Control.Concurrent.STM ( atomically, orElse
                              , TChan, newTChanIO, readTChan, peekTChan, writeTChan
                              , TVar, newTVarIO, readTVar, modifyTVar
                              , TMVar, tryPutTMVar, newEmptyTMVarIO, takeTMVar )
import Control.Exception ( SomeException, AsyncException, Handler(Handler), handle, bracket, throwIO, catches, onException )
import Data.MessagePack
import Data.Serialize hiding (label)
import Data.Int ( Int64 )
import Data.Time.Clock.POSIX ( getPOSIXTime )
import System.Random ( randomRIO )

import Network.Fluent.Logger.Packable
import Network.Fluent.Logger.ForkWrapper (forkIOUnmasked)

-- | Wrap close / sClose (deprecated)
close :: NS.Socket -> IO ()
#if MIN_VERSION_network(2,4,0)
close = NS.close
#else
close = NS.sClose
#endif

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
      onErr e = close sock >> throwIO e
  handle onErr $ do
    NS.connect sock $ NS.addrAddress addr
    return sock

type CloseFlag = TMVar ()

setFlagWhenClose :: NS.Socket -> CloseFlag -> IO ()
setFlagWhenClose sock flag = do
  received <- recv sock 256 `onException` setFlag
  if LBS.length received == 0
    then setFlag
    else setFlagWhenClose sock flag
  where
    setFlag = atomically $ void $ tryPutTMVar flag ()

runSender :: FluentLoggerSender -> IO ()
runSender logger = forever $ filterException $ bracket (connectFluent logger) close handleSocket where
  passAsyncException :: AsyncException -> IO a
  passAsyncException e = throwIO e
  dropOtherExceptions :: SomeException -> IO ()
  dropOtherExceptions _ = return ()
  filterException :: IO () -> IO ()
  filterException action = catches action [Handler passAsyncException, Handler dropOtherExceptions]
  handleSocket sock = do
    flag <- newEmptyTMVarIO
    bracket (forkIOUnmasked $ setFlagWhenClose sock flag) killThread (const $ sendFluent logger sock flag)

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


data SenderEvent = SenderNewData LBS.ByteString
                 | SenderSocketClose

sendFluent :: FluentLoggerSender -> NS.Socket -> CloseFlag -> IO ()
sendFluent logger sock flag = toSender where
    chan = fluentLoggerSenderChan logger
    buffered = fluentLoggerSenderBuffered logger
    toSender = do
      event <- atomically $ (takeTMVar flag >> return SenderSocketClose) `orElse` (fmap SenderNewData $ peekTChan chan)
      case event of
        SenderSocketClose -> return ()
        SenderNewData entry -> do
          sendAll sock entry
          atomically $ do
            void $ readTChan chan
            modifyTVar buffered (subtract $ LBS.length entry)
          sendFluent logger sock flag

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
  tid <- forkIOUnmasked $ runSender sender
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
-- Since 0.2.0.0
--
post :: Packable a => FluentLogger -> BS.ByteString -> a -> IO ()
post logger label obj = do
  time <- getCurrentEpochTime
  postWithTime logger label time obj

-- | Post a message with given time.
--
-- Since 0.2.0.0
--
postWithTime :: Packable a => FluentLogger -> BS.ByteString -> Int -> a -> IO ()
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
