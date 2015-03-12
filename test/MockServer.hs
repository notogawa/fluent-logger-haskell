{-# LANGUAGE OverloadedStrings #-}
module MockServer
    (
      MockServer
    , mockServerHost
    , mockServerPort
    , withMockServer
    , recvMockServer
    , getConnectionCount
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

import Network.Fluent.Logger.Packable (pack)
import Network.Fluent.Logger.Unpackable (Unpackable, unpack)

data MockServer a = MockServer { mockServerChan :: Chan a
                               , mockServerThread :: ThreadId
                               , mockServerConnectionCount :: MVar Int
                               }

mockServerHost :: ByteString
mockServerHost = "127.0.0.1"

mockServerPort :: Int
mockServerPort = 24224

mockServerSettings :: ServerSettings
mockServerSettings = serverSettings mockServerPort "*"

app :: (MonadIO m, MonadThrow m, Serialize a) => Chan a -> MVar Int -> AppData -> m ()
app chan mvconn ad = do
  liftIO $ modifyMVar_ mvconn (return . (+ 1))
  appSource ad $= conduitGet get $$ sinkChan chan mvconn

sinkChan :: (MonadIO m, Serialize a) => Chan a -> MVar Int -> Sink a m ()
sinkChan chan mvconn = do
  mx <- await
  case mx of
    Nothing -> liftIO $ modifyMVar_ mvconn (return . (subtract 1))
    Just x  -> liftIO (writeChan chan x) >> sinkChan chan mvconn

withMockServer :: Serialize a => (MockServer a -> IO ()) -> IO ()
withMockServer = bracket runMockServer stopMockServer

recvMockServer :: Unpackable b => MockServer Object -> IO b
recvMockServer server = fmap unpack $ readChan (mockServerChan server)

runMockServer :: Serialize a => IO (MockServer a)
runMockServer = do
  chan <- newChan
  mvconn <- newMVar 0
  tid <- forkIO $ runTCPServer mockServerSettings $ app chan mvconn
  threadDelay 10000
  return MockServer { mockServerChan = chan
                    , mockServerThread = tid
                    , mockServerConnectionCount = mvconn
                    }

stopMockServer :: Serialize a => MockServer a -> IO ()
stopMockServer server = do
  killThread $ mockServerThread server
  threadDelay 10000

getConnectionCount :: MockServer a -> IO Int
getConnectionCount = readMVar . mockServerConnectionCount
