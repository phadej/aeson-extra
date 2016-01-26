{-# LANGUAGE CPP #-}
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
  -- * Generic maps
  M(..),
  FromJSONKey(..),
  parseIntegralJSONKey,
  FromJSONMap(..),
  ToJSONKey(..),
  ToJSONMap(..),
#if MIN_VERSION_base(4,7,0)
  -- * Symbol tag
  SymTag(..),
  -- * Singleton object
  SingObject(..),
  mkSingObject,
  getSingObject,
#endif
  -- * CollapsedList
  CollapsedList(..),
  getCollapsedList,
  parseCollapsedList,
  -- * UTCTime
  U(..),
  Z(..),
  -- * Re-exports
  module Data.Aeson.Compat,
  ) where

import Prelude        ()
import Prelude.Compat

import Data.Aeson.Compat

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Aeson.Extra.CollapsedList
import Data.Aeson.Extra.Map
import Data.Aeson.Extra.Time

#if MIN_VERSION_base(4,7,0)
import Data.Aeson.Extra.SingObject
import Data.Aeson.Extra.SymTag
#endif

-- | Like 'encode', but produces strict 'BS.ByteString'.
--
-- /Since: 0.2.3.0/
encodeStrict :: ToJSON a => a -> BS.ByteString
encodeStrict = LBS.toStrict . encode
