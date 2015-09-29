{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aeson.Extra
-- Copyright   :  (C) 2015 Oleg Grenrus
-- License     :  BSD3
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- More or less useful newtypes for writing 'FromJSON' & 'ToJSON' instances
module Data.Aeson.Extra (
  -- * Generic maps
  M(..),
  FromJSONKey(..),
  parseIntegralJSONKey,
  FromJSONMap(..),
  ToJSONKey(..),
  ToJSONMap(..),
  -- * Symbol tag
  SymTag(..),
  -- * Singleton object
  SingObject(..),
  mkSingObject,
  getSingObject,
  -- * CollapsedList
  CollapsedList(..),
  getCollapsedList,
  parseCollapsedList,
  -- * Re-exports
  module Data.Aeson.Compat,
  ) where

#if !MIN_VERSION_base(4,8,0)
import           Data.Foldable (Foldable)
import           Data.Traversable (Traversable, traverse)
#endif

import           Control.Applicative
import           Data.Monoid
import           Data.Aeson.Compat
import           Data.Aeson.Types hiding ((.:?))
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as H
import           Data.Hashable (Hashable)
import qualified Data.Map as Map
import           Data.Proxy
import           Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as T
import           GHC.TypeLits

-- | A wrapper type to parse arbitrary maps
--
-- > λ > decode "{\"1\": 1, \"2\": 2}" :: Maybe (M (H.HashMap Int Int))
-- > Just (M {getMap = fromList [(1,1),(2,2)]})
newtype M a = M { getMap :: a }
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

class FromJSONKey a where
  parseJSONKey :: Text -> Parser a

instance FromJSONKey Text where parseJSONKey = pure
instance FromJSONKey TL.Text where parseJSONKey = pure . TL.fromStrict
instance FromJSONKey String where parseJSONKey = pure . T.unpack
instance FromJSONKey Int where parseJSONKey = parseIntegralJSONKey
instance FromJSONKey Integer where parseJSONKey = parseIntegralJSONKey

parseIntegralJSONKey :: Integral a => Text -> Parser a
parseIntegralJSONKey t = case (T.signed T.decimal) t of
  Right (v, left) | T.null left  -> pure v
                  | otherwise    -> fail $ "Garbage left: " <> T.unpack left
  Left err                       -> fail err

class FromJSONMap m k v | m -> k v where
  parseJSONMap :: H.HashMap Text Value -> Parser m

instance (Eq k, Hashable k, FromJSONKey k, FromJSON v) => FromJSONMap (H.HashMap k v) k v where
  parseJSONMap = fmap H.fromList . traverse f . H.toList
    where f (k, v) = (,) <$> parseJSONKey k <*> parseJSON v

instance (Ord k, FromJSONKey k, FromJSON v) => FromJSONMap (Map.Map k v) k v where
  parseJSONMap = fmap Map.fromList . traverse f . H.toList
    where f (k, v) = (,) <$> parseJSONKey k <*> parseJSON v

instance (FromJSONMap m k v) => FromJSON (M m) where
  parseJSON v = M <$> withObject "Map" parseJSONMap v


class ToJSONKey a where
  toJSONKey :: a -> Text

instance ToJSONKey Text where toJSONKey = id
instance ToJSONKey TL.Text where toJSONKey = TL.toStrict
instance ToJSONKey String where toJSONKey = T.pack
instance ToJSONKey Int where toJSONKey = T.pack . show
instance ToJSONKey Integer where toJSONKey = T.pack . show

class ToJSONMap m k v | m -> k v where
  toJSONMap :: m -> H.HashMap Text Value

instance (ToJSONKey k, ToJSON v) => ToJSONMap (H.HashMap k v) k v where
  toJSONMap = H.fromList . fmap f . H.toList
    where f (k, v) = (toJSONKey k, toJSON v)

instance (ToJSONKey k, ToJSON v) => ToJSONMap (Map.Map k v) k v where
  toJSONMap = H.fromList . fmap f . Map.toList
    where f (k, v) = (toJSONKey k, toJSON v)

instance (ToJSONMap m k v) => ToJSON (M m) where
  toJSON (M m) = Object (toJSONMap m)

-- | Singleton string encoded and decoded as ifself.
--
-- > λ> encode (SymTag :: SymTag "foobar")
-- > "\"foobar\""
--
-- > decode "\"foobar\"" :: Maybe (SymTag "foobar")
-- > Just Sym
--
-- > decode "\"foobar\"" :: Maybe (SymTag "barfoo")
-- > Nothing
data SymTag (s :: Symbol) = SymTag
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance KnownSymbol s => FromJSON (SymTag s) where
  parseJSON (String t)
    | T.unpack t == symbolVal (Proxy :: Proxy s) = pure SymTag
  parseJSON v = typeMismatch ("SymTag " ++ show (symbolVal (Proxy :: Proxy s))) v

instance KnownSymbol s => ToJSON (SymTag s) where
#if MIN_VERSION_aeson (0,10,0)
  toEncoding _ = toEncoding (symbolVal (Proxy :: Proxy s))
#endif
  toJSON _ = toJSON (symbolVal (Proxy :: Proxy s))


-- | Singleton value object
--
-- > λ > decode "{\"value\": 42 }" :: Maybe (SingObject "value" Int)
-- > Just (SingObject 42)
--
-- > λ > encode (SingObject 42 :: SingObject "value" Int)
-- > "{\"value\":42}"
newtype SingObject (s ::Symbol) a = SingObject a
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

mkSingObject :: Proxy s -> a -> SingObject s a
mkSingObject _ = SingObject

getSingObject :: Proxy s -> SingObject s a -> a
getSingObject _ (SingObject x) = x

instance (KnownSymbol s, FromJSON a) => FromJSON (SingObject s a) where
  parseJSON = withObject ("SingObject "<> show key) $ \obj -> 
    SingObject <$> obj .: T.pack key
    where key = symbolVal (Proxy :: Proxy s)

instance (KnownSymbol s, ToJSON a) => ToJSON (SingObject s a) where
#if MIN_VERSION_aeson(0,10,0)
  toEncoding (SingObject x) = pairs (T.pack key .= x)
    where key = symbolVal (Proxy :: Proxy s)
#endif
  toJSON (SingObject x) = object [T.pack key .= x]
    where key = symbolVal (Proxy :: Proxy s)


-- | Collapsed list, singleton is represented as the value itself in JSON encoding.
--
-- > λ > decode "null" :: Maybe (CollapsedList [Int] Int)
-- > Just (CollapsedList [])
-- > λ > decode "42" :: Maybe (CollapsedList [Int] Int)
-- > Just (CollapsedList [42])
-- > λ > decode "[1, 2, 3]" :: Maybe (CollapsedList [Int] Int)
-- > Just (CollapsedList [1,2,3])
--
-- > λ > encode (CollapsedList ([] :: [Int]))
-- > "null"
-- > λ > encode (CollapsedList ([42] :: [Int]))
-- > "42"
-- > λ > encode (CollapsedList ([1, 2, 3] :: [Int]))
-- > "[1,2,3]"
--
-- Documentation rely on @f@ 'Alternative' instance behaving like lists'.
newtype CollapsedList f a = CollapsedList (f a)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

getCollapsedList :: CollapsedList f a -> f a
getCollapsedList (CollapsedList l) = l

instance (FromJSON a, FromJSON (f a), Alternative f) => FromJSON (CollapsedList f a) where
  parseJSON Null         = pure (CollapsedList Control.Applicative.empty)
  parseJSON v@(Array _)  = CollapsedList <$> parseJSON v
  parseJSON v            = CollapsedList . pure <$> parseJSON v

instance (ToJSON a, ToJSON (f a), Foldable f) => ToJSON (CollapsedList f a) where
#if MIN_VERSION_aeson (0,10,0)
  toEncoding (CollapsedList l) =
    case Foldable.toList l of
      []   -> toEncoding Null
      [x]  -> toEncoding x
      _    -> toEncoding l
#endif
  toJSON (CollapsedList l) =
    case Foldable.toList l of
      []   -> toJSON Null
      [x]  -> toJSON x
      _    -> toJSON l

-- | Parses possibly collapsed array value from the object's field.
--
-- > λ > newtype V = V [Int] deriving (Show)
-- > λ > instance FromJSON V where parseJSON = withObject "V" $ \obj -> V <$> collapsedList obj "value"
-- > λ > decode "{}" :: Maybe V
-- > Just (V [])
-- > λ > decode "{\"value\": null}" :: Maybe V
-- > Just (V [])
-- > λ > decode "{\"value\": 42}" :: Maybe V
-- > Just (V [42])
-- > λ > decode "{\"value\": [1, 2, 3, 4]}" :: Maybe V
-- > Just (V [1,2,3,4])
parseCollapsedList :: (FromJSON a, FromJSON (f a), Alternative f) => Object -> Text -> Parser (f a)
parseCollapsedList obj key =
  case H.lookup key obj of
    Nothing   -> pure Control.Applicative.empty
#if MIN_VERSION_aeson(0,10,0) 
    Just v    -> modifyFailure addKeyName $ (getCollapsedList <$> parseJSON v) -- <?> Key key
  where
    addKeyName = (("failed to parse field " <> T.unpack key <> ": ") <>)
#else
    Just v    -> getCollapsedList <$> parseJSON v
#endif
