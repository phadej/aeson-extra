{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aeson.Extra.Time
-- Copyright   :  (C) 2015-2016 Oleg Grenrus
-- License     :  BSD3
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Time tools
module Data.Aeson.Extra.Time (
    U(..),
    Z(..),
    )where

import Prelude ()
import Prelude.Compat

import Data.Aeson.Compat
import Data.Time         (UTCTime, ZonedTime)
import Data.Typeable     (Typeable)

#if !MIN_VERSION_aeson (0,10,0)
import Data.Text (Text)

#if !MIN_VERSION_aeson_compat(0,3,5)
import Data.Aeson.Types (Parser)
#endif

import qualified Data.Time.Parsers as TimeParsers
import qualified Text.Parsec       as Parsec
#endif

-- | A type to parse 'UTCTime'
--
-- 'FromJSON' instance accepts for example:
--
-- @
-- 2015-09-07T08:16:40.807Z
-- 2015-09-07 11:16:40.807 +03:00
-- @
--
-- Latter format is accepted by @aeson@ staring from version @0.10.0.0@.
--
-- See <https://github.com/bos/aeson/blob/4667ef1029a373cf4510f7deca147c357c6d8947/Data/Aeson/Parser/Time.hs#L150>
--
-- /Since: aeson-extra-0.2.2.0/
newtype U = U { getU :: UTCTime }
  deriving (Eq, Ord, Show, Read, Typeable)

instance ToJSON U where
  toJSON = toJSON . getU
#if MIN_VERSION_aeson (0,10,0)
  toEncoding = toEncoding . getU
#endif

instance FromJSON U where
#if MIN_VERSION_aeson (0,10,0)
  parseJSON = fmap U . parseJSON
#else
  parseJSON = withText "UTCTime" (fmap U . run TimeParsers.utcTime)
#endif

-- | A type to parse 'ZonedTime'
--
-- /Since: aeson-extra-0.2.2.0/
newtype Z = Z { getZ :: ZonedTime }
  deriving (Show, Read, Typeable)

instance ToJSON Z where
  toJSON = toJSON . getZ
#if MIN_VERSION_aeson (0,10,0)
  toEncoding = toEncoding . getZ
#endif

instance FromJSON Z where
#if MIN_VERSION_aeson (0,10,0)
  parseJSON = fmap Z . parseJSON
#else
  parseJSON = withText "ZonedTime" (fmap Z . run TimeParsers.zonedTime)
#endif

#if !MIN_VERSION_aeson (0,10,0)
-- | Run a 'parsers' parser as an aeson parser.
run :: Parsec.Parsec Text () a -> Text -> Parser a
run p t = case Parsec.parse (p <* Parsec.eof) "" t of
            Left err -> fail $ "could not parse date: " ++ show err
            Right r  -> return r
#endif
