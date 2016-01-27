{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aeson.Extra.TH
-- Copyright   :  (C) 2015-2016 Oleg Grenrus
-- License     :  BSD3
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- In addition to 'mkValue' and 'mkValue'' helpers,
-- this module exports 'Lift' 'Value' orphan instance
module Data.Aeson.Extra.TH (
    mkValue,
    mkValue',
    ) where

import Control.Arrow (first)
import Data.Aeson.Compat
import Data.Scientific (scientific, coefficient, base10Exponent)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

-- | Create a 'Value' from string representation.
-- 
-- This is useful in tests.
--
-- /Since: aeson-extra-0.3.1.0/
mkValue :: String -> Q Exp
mkValue s = case eitherDecodeStrict' bs :: Either String Value of
    Left err -> fail $ "mkValue: " ++ err
    Right v  -> [| v |]
  where bs = TE.encodeUtf8 $ T.pack s

-- | Like 'mkValue', but replace single quotes with double quotes before.
--
-- > > $(mkValue' "{'a': 2 }")
-- > Object (fromList [("a",Number 2.0)])
--
-- /Since: aeson-extra-0.3.1.0/
mkValue' :: String -> Q Exp
mkValue' = mkValue . map f
  where f '\'' = '"'
        f x    = x

-- | From 'aeson-extra'
--
-- /Since: aeson-extra-0.3.1.0/
instance Lift Value where
    lift Null = [| Null |]
    lift (Bool b) = [| Bool b |]
    lift (Number n) = [| Number (scientific c e) |]
      where
        c = coefficient n
        e = base10Exponent n
    lift (String t) = [| String (T.pack s) |]
      where s = T.unpack t
    lift (Array a) = [| Array (V.fromList a') |]
      where a' = V.toList a
    lift (Object o) = [| Object (HM.fromList . map (first T.pack) $ o') |]
      where o' = map (first T.unpack) . HM.toList $ o
