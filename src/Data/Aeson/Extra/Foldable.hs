{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, TypeFamilies, DeriveFoldable, DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aeson.Extra.Foldable
-- Copyright   :  (C) 2015-2016 Oleg Grenrus
-- License     :  BSD3
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Helps writing recursive algorithms on 'Value', for example:
--
-- @
-- stripNulls :: Value -> Value
-- stripNulls = 'cata' ('embed' . f)
--  where
--    f (ObjectF a) = ObjectF $ HM.filter (== Null) a
--    f x = x
-- @
module Data.Aeson.Extra.Foldable (
    ValueF(..),
    ObjectF,
    ArrayF,
    ) where

import Prelude        ()
import Prelude.Compat

import Data.Aeson.Compat
import Data.Functor.Foldable
import Data.HashMap.Strict (HashMap)
import Data.Data (Data)
import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.Typeable (Typeable)
import Data.Vector (Vector)

import qualified Data.Functor.Foldable as F

-- | A JSON \"object\" (key\/value map).
--
-- /Since: aeson-extra-0.3.1.0/
type ObjectF a = HashMap Text a

-- | A JSON \"array\" (sequence).
--
-- /Since: aeson-extra-0.3.1.0/
type ArrayF a = Vector a

-- | An algebra of 'Value'
--
-- /Since: aeson-extra-0.3.1.0/
data ValueF a
    = ObjectF (ObjectF a)
    | ArrayF !(ArrayF a)
    | StringF !Text
    | NumberF !Scientific
    | BoolF !Bool
    | NullF
    deriving (Eq, Read, Show, Typeable, Data, Functor, Prelude.Compat.Foldable, Traversable)

type instance Base Value = ValueF

instance F.Foldable Value where
    project (Object o) = ObjectF o
    project (Array a)  = ArrayF a
    project (String s) = StringF s
    project (Number n) = NumberF n
    project (Bool b)   = BoolF b
    project Null       = NullF

instance F.Unfoldable Value where
    embed (ObjectF o) = Object o
    embed (ArrayF a)  = Array a
    embed (StringF s) = String s
    embed (NumberF n) = Number n
    embed (BoolF b)   = Bool b
    embed NullF       = Null
