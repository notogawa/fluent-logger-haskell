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
import Data.ByteString ( ByteString )
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Exception
import Data.MessagePack ( Unpackable, get )
import Data.Attoparsec
import Data.Monoid

data MockServer a = MockServer { mockServerChan :: Chan a
                               , mockServerThread :: ThreadId
                               }

mockServerHost :: ByteString
mockServerHost = "127.0.0.1"

mockServerPort :: Int
mockServerPort = 24224

mockServerSettings :: ServerSettings IO
mockServerSettings = serverSettings mockServerPort HostAny

app :: (MonadIO m, Unpackable a) => Chan a -> AppData m -> m ()
app chan ad = appSource ad $$ sinkChan chan ""

sinkChan :: (MonadIO m, Unpackable a) => Chan a -> ByteString -> Sink ByteString m ()
sinkChan chan carry = do
  mx <- await
  case mx of
    Nothing -> return ()
    Just x  -> parseAsPossible chan (carry <> x) >>= sinkChan chan

parseAsPossible :: (MonadIO m, Unpackable a) => Chan a -> ByteString -> m ByteString
parseAsPossible chan src =
    case parse get src of
      Done t r -> liftIO (writeChan chan r) >> parseAsPossible chan t
      _ -> return src

withMockServer :: Unpackable a => (MockServer a -> IO ()) -> IO ()
withMockServer = bracket runMockServer stopMockServer

recvMockServer :: Unpackable a => MockServer a -> IO a
recvMockServer server = readChan (mockServerChan server)

runMockServer :: Unpackable a => IO (MockServer a)
runMockServer = do
  chan <- newChan
  tid <- forkIO $ runTCPServer mockServerSettings $ app chan
  threadDelay 10000
  return MockServer { mockServerChan = chan
                    , mockServerThread = tid
                    }

stopMockServer :: Unpackable a => MockServer a -> IO ()
stopMockServer server = do
  killThread $ mockServerThread server
  threadDelay 10000
