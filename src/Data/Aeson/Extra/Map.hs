{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aeson.Extra.Map
-- Copyright   :  (C) 2015-2016 Oleg Grenrus
-- License     :  BSD3
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- More or less useful newtypes for writing 'FromJSON' & 'ToJSON' instances
module Data.Aeson.Extra.Map {-# DEPRECATED "Use aeson-1 TOJSONKey etc functionality" #-} (
  M(..),
  FromJSONKey(..),
  parseIntegralJSONKey,
  FromJSONMap(..),
  ToJSONKey(..),
  ToJSONMap(..),
  ) where

import Prelude ()
import Prelude.Compat

import Data.Aeson.Compat
       (FromJSON (..), ToJSON (..), Value (..), withObject)
import Data.Aeson.Types      (Parser)
import Data.Hashable         (Hashable)
import Data.Semigroup.Compat ((<>))
import Data.Text             (Text)
import Data.Typeable         (Typeable)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as Map
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as LT
import qualified Data.Text.Read      as T

-- | A wrapper type to parse arbitrary maps
--
-- > λ > decode "{\"1\": 1, \"2\": 2}" :: Maybe (M (H.HashMap Int Int))
-- > Just (M {getMap = fromList [(1,1),(2,2)]})
newtype M a = M { getMap :: a }
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Typeable)
{-# DEPRECATED M "Use aeson-1 TOJSONKey etc functionality" #-}

class FromJSONKey a where
  parseJSONKey :: Text -> Parser a

instance FromJSONKey Text where parseJSONKey = pure
instance FromJSONKey LT.Text where parseJSONKey = pure . LT.fromStrict
instance FromJSONKey String where parseJSONKey = pure . T.unpack
instance FromJSONKey Int where parseJSONKey = parseIntegralJSONKey
instance FromJSONKey Integer where parseJSONKey = parseIntegralJSONKey

parseIntegralJSONKey :: Integral a => Text -> Parser a
parseIntegralJSONKey t = case T.signed T.decimal t of
  Right (v, left) | T.null left  -> pure v
                  | otherwise    -> fail $ "Garbage left: " <> T.unpack left
  Left err                       -> fail err

class FromJSONMap m k v | m -> k v where
  parseJSONMap :: HM.HashMap Text Value -> Parser m

instance (Eq k, Hashable k, FromJSONKey k, FromJSON v) => FromJSONMap (HM.HashMap k v) k v where
  parseJSONMap = fmap HM.fromList . traverse f . HM.toList
    where f (k, v) = (,) <$> parseJSONKey k <*> parseJSON v

instance (Ord k, FromJSONKey k, FromJSON v) => FromJSONMap (Map.Map k v) k v where
  parseJSONMap = fmap Map.fromList . traverse f . HM.toList
    where f (k, v) = (,) <$> parseJSONKey k <*> parseJSON v

instance (FromJSONMap m k v) => FromJSON (M m) where
  parseJSON v = M <$> withObject "Map" parseJSONMap v


class ToJSONKey a where
  toJSONKey :: a -> Text

instance ToJSONKey Text where toJSONKey = id
instance ToJSONKey LT.Text where toJSONKey = LT.toStrict
instance ToJSONKey String where toJSONKey = T.pack
instance ToJSONKey Int where toJSONKey = T.pack . show
instance ToJSONKey Integer where toJSONKey = T.pack . show

class ToJSONMap m k v | m -> k v where
  toJSONMap :: m -> HM.HashMap Text Value

instance (ToJSONKey k, ToJSON v) => ToJSONMap (HM.HashMap k v) k v where
  toJSONMap = HM.fromList . fmap f . HM.toList
    where f (k, v) = (toJSONKey k, toJSON v)

instance (ToJSONKey k, ToJSON v) => ToJSONMap (Map.Map k v) k v where
  toJSONMap = HM.fromList . fmap f . Map.toList
    where f (k, v) = (toJSONKey k, toJSON v)

instance (ToJSONMap m k v) => ToJSON (M m) where
  toJSON (M m) = Object (toJSONMap m)
