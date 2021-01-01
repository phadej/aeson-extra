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

import Prelude ()
import Prelude.Compat

import Control.DeepSeq       (NFData (..))
import Data.Aeson
import Data.Aeson.Encoding   (pair)
import Data.Aeson.Internal   (JSONPathElement (Key))
import Data.Proxy            (Proxy (..))
import Data.Typeable         (Typeable)
import GHC.TypeLits          (KnownSymbol, Symbol, symbolVal)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T

-- | Singleton value object
--
-- > λ > decode "{\"value\": 42 }" :: Maybe (SingObject "value" Int)
-- > Just (SingObject 42)
--
-- > λ > encode (SingObject 42 :: SingObject "value" Int)
-- > "{\"value\":42}"
--
-- /Available with: base >=4.7/
newtype SingObject (s :: Symbol) a = SingObject a
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Typeable)

mkSingObject :: Proxy s -> a -> SingObject s a
mkSingObject _ = SingObject

getSingObject :: Proxy s -> SingObject s a -> a
getSingObject _ (SingObject x) = x

instance KnownSymbol s => FromJSON1 (SingObject s) where
    liftParseJSON p _ = withObject ("SingObject "<> show key) $ \obj ->
        case HM.lookup key obj of
            Nothing -> fail $ "key " ++ show key ++ " not present"
            Just v  -> SingObject <$> p v <?> Key key
     where
        key = T.pack $ symbolVal (Proxy :: Proxy s)

instance KnownSymbol s => ToJSON1 (SingObject s) where
    liftToJSON     to _ (SingObject x) =
        object [ key .= to x]
      where
        key = T.pack $ symbolVal (Proxy :: Proxy s)
    liftToEncoding to _ (SingObject x) =
        pairs $ pair key $ to x
      where
        key = T.pack $ symbolVal (Proxy :: Proxy s)

instance  (KnownSymbol s, FromJSON a) => FromJSON (SingObject s a) where
    parseJSON = parseJSON1

instance (KnownSymbol s, ToJSON a) => ToJSON (SingObject s a) where
    toJSON     = toJSON1
    toEncoding = toEncoding1

-- | @since 0.4.1.0
instance NFData a => NFData (SingObject s a) where
    rnf (SingObject x) = rnf x
