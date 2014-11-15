{-# LANGUAGE FlexibleInstances, IncoherentInstances, TypeSynonymInstances #-}
-- | For compatibility with msgpack
module Network.Fluent.Logger.Packable (Packable(..)) where

import Data.MessagePack
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

-- | MessagePackable
--
-- Since 0.2.0.0
--
class Packable a where
    pack :: a -> Object

instance Packable Object where
    pack = id

instance Packable () where
    pack = const ObjectNil

instance Packable Int where
    pack = ObjectInt . fromIntegral

instance Packable String where
    pack = ObjectString . T.pack

instance Packable BS.ByteString where
    pack = ObjectBinary

instance Packable LBS.ByteString where
    pack = ObjectBinary . LBS.toStrict

instance Packable T.Text where
    pack = ObjectString

instance Packable LT.Text where
    pack = ObjectString . T.pack . LT.unpack

instance Packable a => Packable [a] where
    pack = ObjectArray . map pack

instance Packable a => Packable (V.Vector a) where
    pack = ObjectArray . map pack . V.toList

instance (Packable a1, Packable a2) => Packable (a1, a2) where
    pack (a1, a2) = ObjectArray [pack a1, pack a2]

instance (Packable a1, Packable a2, Packable a3) => Packable (a1, a2, a3) where
    pack (a1, a2, a3) = ObjectArray [pack a1, pack a2, pack a3]

instance (Packable a1, Packable a2, Packable a3, Packable a4) => Packable (a1, a2, a3, a4) where
    pack (a1, a2, a3, a4) = ObjectArray [pack a1, pack a2, pack a3, pack a4]

instance (Packable a1, Packable a2, Packable a3, Packable a4, Packable a5) => Packable (a1, a2, a3, a4, a5) where
    pack (a1, a2, a3, a4, a5) = ObjectArray [pack a1, pack a2, pack a3, pack a4, pack a5]

instance (Packable k, Packable v) => Packable (M.Map k v) where
    pack = ObjectMap . (M.foldWithKey (\k v -> M.insert (pack k) (pack v)) M.empty)
