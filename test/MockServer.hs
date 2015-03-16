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
import qualified Data.Set as S

import Network.Fluent.Logger.Packable (pack)
import Network.Fluent.Logger.Unpackable (Unpackable, unpack)

data MockServer a = MockServer { mockServerChan :: Chan a
                               , mockServerThread :: ThreadId
                               , mockServerConnectionThreads :: MVar (S.Set ThreadId)
                               }

mockServerHost :: ByteString
mockServerHost = "127.0.0.1"

mockServerPort :: Int
mockServerPort = 24224

mockServerSettings :: ServerSettings
mockServerSettings = serverSettings mockServerPort "*"

app :: (MonadIO m, MonadThrow m, Serialize a) => Chan a -> MVar (S.Set ThreadId) -> AppData -> m ()
app chan mvconn ad = do
  myid <- liftIO myThreadId
  liftIO $ modifyMVar_ mvconn (return . S.insert myid)
  appSource ad $= conduitGet get $$ (addCleanup (unregister myid) $ sinkChan chan) where
    unregister myid _ = liftIO $ modifyMVar_ mvconn (return . S.delete myid)

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
  mvconn <- newMVar S.empty
  tid <- forkIO $ runTCPServer mockServerSettings $ app chan mvconn
  threadDelay 10000
  return MockServer { mockServerChan = chan
                    , mockServerThread = tid
                    , mockServerConnectionThreads = mvconn
                    }

stopMockServer :: Serialize a => MockServer a -> IO ()
stopMockServer server = do
  killThread $ mockServerThread server
  (mapM_ killThread . S.toList) =<< (swapMVar (mockServerConnectionThreads server) S.empty)
  threadDelay 10000

getConnectionCount :: MockServer a -> IO Int
getConnectionCount s = fmap S.size $ readMVar $ mockServerConnectionThreads s
