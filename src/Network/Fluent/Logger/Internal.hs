{-# LANGUAGE FlexibleInstances, IncoherentInstances, TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Network.Fluent.Logger.Internal where

import Control.Exception
import Data.Typeable
import Data.MessagePack
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

data UnpackError =
  UnpackError String
  deriving (Show, Typeable)

instance Exception UnpackError

-- for compatibility with msgpack

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

class Unpackable a where
  unpack :: Object -> a

instance Unpackable Object where
  unpack = id

instance Unpackable () where
  unpack ObjectNil = ()
  unpack x = throw $ UnpackError $ "invalid for nil: " ++ show x

instance Unpackable Int where
  unpack (ObjectInt x) = fromIntegral x
  unpack x = throw $ UnpackError $ "invalid for int: " ++ show x

instance Unpackable String where
  unpack (ObjectString x) = T.unpack x
  unpack x = throw $ UnpackError $ "invalid for string: " ++ show x

instance Unpackable BS.ByteString where
  unpack (ObjectBinary x) = x
  unpack x = throw $ UnpackError $ "invalid for binary: " ++ show x

instance Unpackable LBS.ByteString where
  unpack (ObjectBinary x) = LBS.fromStrict x
  unpack x = throw $ UnpackError $ "invalid for binary: " ++ show x

instance Unpackable T.Text where
  unpack (ObjectString x) = x
  unpack x = throw $ UnpackError $ "invalid for string: " ++ show x

instance Unpackable LT.Text where
  unpack (ObjectString x) = LT.pack . T.unpack $ x
  unpack x = throw $ UnpackError $ "invalid for string: " ++ show x

instance (Unpackable a1, Unpackable a2) => Unpackable (a1, a2) where
  unpack (ObjectArray (a1:a2:[])) = (unpack a1, unpack a2)
  unpack x = throw $ UnpackError $ "invalid for array: " ++ show x

instance (Unpackable a1, Unpackable a2, Unpackable a3) => Unpackable (a1, a2, a3) where
  unpack (ObjectArray (a1:a2:a3:[])) = (unpack a1, unpack a2, unpack a3)
  unpack x = throw $ UnpackError $ "invalid for array: " ++ show x

instance (Unpackable a1, Unpackable a2, Unpackable a3, Unpackable a4) => Unpackable (a1, a2, a3, a4) where
  unpack (ObjectArray (a1:a2:a3:a4:[])) = (unpack a1, unpack a2, unpack a3, unpack a4)
  unpack x = throw $ UnpackError $ "invalid for array: " ++ show x

instance (Unpackable a1, Unpackable a2, Unpackable a3, Unpackable a4, Unpackable a5) => Unpackable (a1, a2, a3, a4, a5) where
  unpack (ObjectArray (a1:a2:a3:a4:a5:[])) = (unpack a1, unpack a2, unpack a3, unpack a4, unpack a5)
  unpack x = throw $ UnpackError $ "invalid for array: " ++ show x

instance (Ord k, Unpackable k, Unpackable v) => Unpackable (M.Map k v) where
  unpack (ObjectMap x) = M.foldWithKey (\k v -> M.insert (unpack k) (unpack v)) M.empty x
  unpack x = throw $ UnpackError $ "invalid for map: " ++ show x
