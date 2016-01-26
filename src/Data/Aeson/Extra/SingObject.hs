{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aeson.Extra.SymTag
-- Copyright   :  (C) 2015-2016 Oleg Grenrus
-- License     :  BSD3
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Data.Aeson.Extra.SingObject (
    SingObject(..),
    mkSingObject,
    getSingObject,
    ) where

import Prelude        ()
import Prelude.Compat

import Data.Aeson.Compat
import Data.Monoid       ((<>))
import Data.Proxy
import Data.Typeable     (Typeable)
import GHC.TypeLits

import qualified Data.Text            as T

-- | Singleton value object
--
-- > λ > decode "{\"value\": 42 }" :: Maybe (SingObject "value" Int)
-- > Just (SingObject 42)
--
-- > λ > encode (SingObject 42 :: SingObject "value" Int)
-- > "{\"value\":42}"
--
-- /Available with: base >=4.7/
newtype SingObject (s ::Symbol) a = SingObject a
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Typeable)

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
