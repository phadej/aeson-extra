{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aeson.Extra
-- Copyright   :  (C) 2015-2016 Oleg Grenrus
-- License     :  BSD3
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- More or less useful newtypes for writing 'FromJSON' & 'ToJSON' instances
module Data.Aeson.Extra (
    -- * Strict encoding
    encodeStrict,
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
    -- * Algebra
    ValueF(..),
    ObjectF,
    ArrayF,
    -- * Merge
    merge,
    lodashMerge,
    -- * Stream
    streamDecode,
    -- * Template Haskell
    mkValue,
    mkValue',
) where

import Prelude ()
import Prelude.Compat

import Data.Aeson

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Aeson.Extra.CollapsedList
import Data.Aeson.Extra.Merge
import Data.Aeson.Extra.Recursive ()
import Data.Aeson.Extra.SingObject
import Data.Aeson.Extra.Stream
import Data.Aeson.Extra.SymTag
import Data.Aeson.Extra.TH

-- | Like 'encode', but produces strict 'BS.ByteString'.
--
-- /Since: 0.2.3.0/
encodeStrict :: ToJSON a => a -> BS.ByteString
encodeStrict = LBS.toStrict . encode
