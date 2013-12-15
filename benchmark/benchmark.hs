{-# LANGUAGE OverloadedStrings #-}
import Network.Fluent.Logger
import Criterion.Main

set :: FluentSettings
set = defaultFluentSettings { fluentSettingsTag = "debug"
                            , fluentSettingsHost = "127.0.0.1"
                            }

main :: IO ()
main = withFluentLogger set $ \logger -> do
         defaultMain [ bgroup "post" [ bench "1 to 1000" $ nfIO $ mapM_ (post logger "loop") [1..1000::Int]
                                     ]
                     ]
