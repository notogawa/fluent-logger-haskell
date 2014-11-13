{-# LANGUAGE OverloadedStrings #-}
module MockServer
    (
      MockServer
    , mockServerHost
    , mockServerPort
    , withMockServer
    , recvMockServer
    ) where

import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Cereal
import Data.ByteString ( ByteString )
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Catch (MonadThrow)
import Control.Exception
import Data.Serialize (Serialize, get)
import Data.MessagePack
import Data.Monoid

import Network.Fluent.Logger.Internal (Unpackable, pack, unpack)

data MockServer a = MockServer { mockServerChan :: Chan a
                               , mockServerThread :: ThreadId
                               }

mockServerHost :: ByteString
mockServerHost = "127.0.0.1"

mockServerPort :: Int
mockServerPort = 24224

mockServerSettings :: ServerSettings
mockServerSettings = serverSettings mockServerPort "*"

app :: (MonadIO m, MonadThrow m, Serialize a) => Chan a -> AppData -> m ()
app chan ad = appSource ad $= conduitGet get $$ sinkChan chan

sinkChan :: (MonadIO m, Serialize a) => Chan a -> Sink a m ()
sinkChan chan = do
  mx <- await
  case mx of
    Nothing -> return ()
    Just x  -> liftIO (writeChan chan x) >> sinkChan chan

withMockServer :: Serialize a => (MockServer a -> IO ()) -> IO ()
withMockServer = bracket runMockServer stopMockServer

recvMockServer :: Unpackable b => MockServer Object -> IO b
recvMockServer server = fmap unpack $ readChan (mockServerChan server)

runMockServer :: Serialize a => IO (MockServer a)
runMockServer = do
  chan <- newChan
  tid <- forkIO $ runTCPServer mockServerSettings $ app chan
  threadDelay 10000
  return MockServer { mockServerChan = chan
                    , mockServerThread = tid
                    }

stopMockServer :: Serialize a => MockServer a -> IO ()
stopMockServer server = do
  killThread $ mockServerThread server
  threadDelay 10000
