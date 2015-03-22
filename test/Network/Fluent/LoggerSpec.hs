{-# LANGUAGE OverloadedStrings #-}
module Network.Fluent.LoggerSpec ( spec ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Time.Clock.POSIX ( getPOSIXTime )
import Data.ByteString ( ByteString )
import Data.MessagePack
import Test.Hspec

import Network.Fluent.Logger
import MockServer

spec :: Spec
spec = do
  describe "post" $ do
    it "posts a message" postPostsMessage
    it "keeps message order" postKeepsMessageOrder
    it "buffers message if server down" postBuffersMessageIfServerDown
    it "buffers message if lost connection" postBuffersMessageIfLostConnection
    -- it "losts message if buffer is over" postLostsMessageIfBufferIsOver
  describe "postWithTime" $ do
    it "posts a message with given time" postWithTimePostsMessageWithGivenTime
  describe "withFluentLogger" $ do
    it "disconnects when the scope is over" withFluentLoggerDisconnect
    it "exits normally even if you don't wait for the message to be received" withFluentLoggerExit


postSettings =
    defaultFluentSettings { fluentSettingsTag = "post"
                          , fluentSettingsHost = mockServerHost
                          , fluentSettingsPort = mockServerPort
                          }

postWithTimeSettings =
    defaultFluentSettings { fluentSettingsTag = "postWithTime"
                          , fluentSettingsHost = mockServerHost
                          , fluentSettingsPort = mockServerPort
                          }

getCurrentEpochTime :: IO Int
getCurrentEpochTime = round <$> getPOSIXTime

shouldHaveConns :: MockServer a -> Int -> IO ()
shouldHaveConns server exp = getConnectionCount server >>= (`shouldBe` exp)

postPostsMessage :: IO ()
postPostsMessage =
    withFluentLogger postSettings $ \logger -> do
      let label = "PostsMessage"
      withMockServer $ \server -> do
        preTime <- getCurrentEpochTime
        post logger label ( "test" :: String )
        postTime <- getCurrentEpochTime
        (tag, time, content) <- recvMockServer server :: IO (ByteString, Int, String)
        tag `shouldBe` "post.PostsMessage"
        content `shouldBe` "test"

postKeepsMessageOrder :: IO ()
postKeepsMessageOrder =
    withFluentLogger postSettings $ \logger -> do
      let label = "KeepsMessageOrder"
      withMockServer $ \server -> do
        let xs = [1..1024::Int]
        mapM_ (post logger label) xs
        forM_ xs $ \n -> do
          (_, _, content) <- recvMockServer server :: IO (ByteString, Int, Int)
          content `shouldBe` n

postBuffersMessageIfServerDown :: IO ()
postBuffersMessageIfServerDown =
    withFluentLogger postSettings $ \logger -> do
      let label = "BuffersMessageIfServerDown"
      post logger label ( 1 :: Int )
      withMockServer $ \server -> do
        post logger label ( 2 :: Int )
        (_, _, content) <- recvMockServer server :: IO (ByteString, Int, Int)
        content `shouldBe` 1
        (_, _, content) <- recvMockServer server :: IO (ByteString, Int, Int)
        content `shouldBe` 2

postBuffersMessageIfLostConnection :: IO ()
postBuffersMessageIfLostConnection =
    withFluentLogger postSettings $ \logger -> do
      let label = "BuffersMessageIfLostConnection"
      withMockServer $ \server -> do
        post logger label ( 1 :: Int )
        (_, _, content) <- recvMockServer server :: IO (ByteString, Int, Int)
        content `shouldBe` 1
        server `shouldHaveConns` 1
      post logger label ( 2 :: Int )
      withMockServer $ \server -> do
        post logger label ( 3 :: Int )
        (_, _, content) <- recvMockServer server :: IO (ByteString, Int, Int)
        content `shouldBe` 2
        server `shouldHaveConns` 1
        (_, _, content) <- recvMockServer server :: IO (ByteString, Int, Int)
        content `shouldBe` 3
        server `shouldHaveConns` 1

postLostsMessageIfBufferIsOver :: IO ()
postLostsMessageIfBufferIsOver =
    withFluentLogger postSettings $ \logger -> do
      let label = "LostsMessageIfBufferIsOver"
      post logger label ( 1 :: Int )
      withMockServer $ \server -> do
        post logger label ( 2 :: Int )
        (_, _, content) <- recvMockServer server :: IO (ByteString, Int, Int)
        content `shouldBe` 2

postWithTimePostsMessageWithGivenTime :: IO ()
postWithTimePostsMessageWithGivenTime =
    withFluentLogger postWithTimeSettings $ \logger -> do
      let label = "PostsMessageWithGivenTime"
      withMockServer $ \server -> do
        postWithTime logger label 123456 ( "test" :: String )
        (tag, time, content) <- recvMockServer server :: IO (ByteString, Int, String)
        tag `shouldBe` "postWithTime.PostsMessageWithGivenTime"
        time `shouldBe` 123456
        content `shouldBe` "test"

withFluentLoggerDisconnect :: IO ()
withFluentLoggerDisconnect =
  withMockServer $ \server -> do
    server `shouldHaveConns` 0
    withFluentLogger postSettings $ \logger -> do
      post logger "hoge" ("foobar" :: String)
      (tag, time, content) <- recvMockServer server :: IO (ByteString, Int, String)
      server `shouldHaveConns` 1
      tag `shouldBe` "post.hoge"
      content `shouldBe` "foobar"
    threadDelay 5000
    server `shouldHaveConns` 0

withFluentLoggerExit :: IO ()
withFluentLoggerExit =
  (withMockServer :: (MockServer Object -> IO ()) -> IO ()) $ \server -> do
    withFluentLogger postSettings $ \logger -> do
      post logger "hoge" ("foobar" :: String)
      -- immediately exit the block without waiting for the message
    threadDelay 5000
    server `shouldHaveConns` 0
