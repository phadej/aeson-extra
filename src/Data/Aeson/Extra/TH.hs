{-# LANGUAGE CPP             #-}
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
-- this module exports 'Lift' 'Value' orphan instance for aeson <0.11
module Data.Aeson.Extra.TH (
    mkValue,
    mkValue',
    ) where

import Language.Haskell.TH

import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

import Data.Aeson

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
