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
{-# LANGUAGE RankNTypes #-}
-- | Fluent Logger with Conduit Interface
module Network.Fluent.Conduit
    ( -- * Sinks
      sinkFluent
    , sinkFluentWithLogger
    ) where

import Network.Fluent.Logger ( FluentSettings, FluentLogger, newFluentLogger, closeFluentLogger, post )
import Control.Monad.IO.Class ( liftIO )
import Data.ByteString ( ByteString )
import Data.Conduit ( MonadResource, bracketP, awaitForever, Consumer )
import Data.MessagePack ( Packable )

-- | Stream all incoming pair ( label, data ) to the given Fluent.
--
-- Since 0.2.0.0
--
sinkFluent :: (MonadResource m, Packable a) => FluentSettings -> Consumer (ByteString, a) m ()
sinkFluent set = bracketP
                 (newFluentLogger set)
                 (liftIO . closeFluentLogger)
                 sinkFluentWithLogger

-- | Stream all incoming pair ( label, data ) to the given Fluent logger.
--
-- Since 0.2.0.0
--
sinkFluentWithLogger :: (MonadResource m, Packable a) => FluentLogger -> Consumer (ByteString, a) m ()
sinkFluentWithLogger logger = awaitForever $ liftIO . uncurry (post logger)
