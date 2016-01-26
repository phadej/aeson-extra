{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aeson.Extra.SymTag
-- Copyright   :  (C) 2015-2016 Oleg Grenrus
-- License     :  BSD3
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Data.Aeson.Extra.SymTag (
    SymTag(..),
    ) where

import Prelude        ()
import Prelude.Compat

import Data.Aeson.Compat
import Data.Aeson.Types  hiding ((.:?))
import Data.Proxy
import GHC.TypeLits

import qualified Data.Text as T

-- | Singleton string encoded and decoded as ifself.
--
-- > λ> encode (SymTag :: SymTag "foobar")
-- > "\"foobar\""
--
-- > decode "\"foobar\"" :: Maybe (SymTag "foobar")
-- > Just SymTag
--
-- > decode "\"foobar\"" :: Maybe (SymTag "barfoo")
-- > Nothing
--
-- /Available with: base >=4.7/
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
