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
	ValueF(..),
	ObjectF,
	ArrayF,
	) where

import Prelude        ()
import Prelude.Compat

import Data.Aeson.Compat
import Data.Aeson.Extra.Foldable
import Data.Functor.Foldable (project, embed)

-- | Generic merge.
--
-- For example <https://lodash.com/docs#merge>:
--
-- @
-- lodashMerge :: Value -> Value -> Value
-- lodashMerge x y = merge lodashMergeAlg x y
-- 
-- lodashMergeAlg :: (a -> a -> a) -> ValueF a -> ValueF a -> ValueF a
-- lodashMergeAlg r a' b' = case (a', b') of
--     (ObjectF a, ObjectF b) -> ObjectF $ alignWith f a b
--     (ArrayF a,  ArrayF b)  -> ArrayF $ alignWith f a b
--     (_,         b)         -> b
--   where f (These x y) = r x y
--         f (This x)    = x
--         f (That x)    = x
-- @
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
