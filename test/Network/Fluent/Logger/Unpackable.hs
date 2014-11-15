{-# LANGUAGE FlexibleInstances, IncoherentInstances, TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | For compatibility with msgpack
module Network.Fluent.Logger.Unpackable where

import Control.Exception
import Data.Typeable
import Data.MessagePack
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

data UnpackError =
    UnpackError String
    deriving (Show, Typeable)

instance Exception UnpackError

-- | Deserializable Type
--
-- Since 0.2.0.0
--
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
