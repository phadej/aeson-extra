{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aeson.Extra.Merge
-- Copyright   :  (C) 2015-2016 Oleg Grenrus
-- License     :  BSD3
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Data.Aeson.Extra.Merge (
    merge,
    mergeA,
    lodashMerge,
    ValueF(..),
    ObjectF,
    ArrayF,
    ) where

import Prelude ()
import Prelude.Compat

import Data.Aeson
import Data.Aeson.Extra.Recursive
import Data.Align                 (alignWith)
import Data.Functor.Foldable      (embed, project)
import Data.These                 (These (..))

-- | Generic merge.
--
-- For example see 'lodashMerge'.
--
-- /Since: aeson-extra-0.3.1.0/
merge :: (forall a. (a -> a -> a) -> ValueF a -> ValueF a -> ValueF a)
       -> Value -> Value -> Value
merge f a b = embed $ f (merge f) (project a) (project b)

-- | Generic merge, in arbitrary context.
--
-- /Since: aeson-extra-0.3.1.0/
mergeA :: Functor f
      => (forall a. (a -> a -> f a) -> ValueF a -> ValueF a -> f (ValueF a))
      -> Value -> Value -> f Value
mergeA f a b = embed <$> f (mergeA f) (project a) (project b)

-- | Example of using 'merge'. see <https://lodash.com/docs#merge>:
--
-- /Note:/ not tested against JavaScript lodash, so may disagree in the results.
--
-- @since 0.4.1.0
lodashMerge :: Value -> Value -> Value
lodashMerge = merge alg
  where
    alg :: (a -> a -> a) -> ValueF a -> ValueF a -> ValueF a
    alg r a' b' = case (a', b') of
        (ObjectF a, ObjectF b) -> ObjectF $ alignWith f a b
        (ArrayF a,  ArrayF b)  -> ArrayF $ alignWith f a b
        (_,         b)         -> b
      where
        f (These x y) = r x y
        f (This x)    = x
        f (That x)    = x
