{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Recurse (recurseTests) where

import Prelude ()
import Prelude.Compat

import Data.Aeson
import Data.Aeson.Extra
import Data.Functor.Foldable (cata, embed)
import Data.List (sort)

import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

empty :: Value
empty = $(mkValue "{}")

sortedInts :: Value
sortedInts = $(mkValue' "{'ints': [1,2,3]}")

sortedKeys :: Value
sortedKeys = $(mkValue' "{'a': 'x', 'b': 'y', 'c': 'z'}")

combined :: Value
combined = $(mkValue' "{'b': 'y', 'c': [1,2,3]}")

nested :: Value
nested = $(mkValue' "{'abc': {'a': 'x', 'b': 'y', 'c': 'z'}, 'ints': [1,2,3], 'value': null}")

recurseTests :: TestTree
recurseTests = testGroup "Recurse examples"
  [ testCase "strip nulls" $ stripNulls $(mkValue' "{'value': null}") @?= empty
  , testCase "sort arrays" $ sortArrays $(mkValue' "{'ints': [2,1,3]}") @?= sortedInts
  , testCase "sort keys" $ sortKeys $(mkValue' "{'b': 'y', 'c': 'z', 'a': 'x'}") @?= sortedKeys

  , testCase "strip nulls . sort arrays . sort keys" $ let f = stripNulls . sortArrays . sortKeys in
        f $(mkValue' "{'b': 'y', 'c': [2,3,1], 'a': null }") @?= combined
  , testCase "strip nulls . sort keys . sort arrays" $ let f = stripNulls . sortKeys . sortArrays in
        f $(mkValue' "{'b': 'y', 'c': [2,3,1], 'a': null }") @?= combined

  , testCase "sort keys . strip nulls . sort arrays" $ let f = sortKeys . stripNulls . sortArrays in
        f $(mkValue' "{'b': 'y', 'c': [2,3,1], 'a': null }") @?= combined
  , testCase "sort keys . sort arrays . strip nulls" $ let f = sortKeys . sortArrays . stripNulls in
        f $(mkValue' "{'b': 'y', 'c': [2,3,1], 'a': null }") @?= combined

  , testCase "sort arrays . sort keys . strip nulls" $ let f = sortArrays . sortKeys . stripNulls in
        f $(mkValue' "{'b': 'y', 'c': [2,3,1], 'a': null }") @?= combined
  , testCase "sort arrays . strip nulls . sort keys" $ let f = sortArrays . stripNulls . sortKeys in
        f $(mkValue' "{'b': 'y', 'c': [2,3,1], 'a': null }") @?= combined

  , testCase "nested sorting; arrays . keys" $ let f = sortArrays . sortKeys in
        f $(mkValue' "{'value': null, 'ints': [2,3,1], 'abc': {'b': 'y', 'c': 'z', 'a': 'x'}}") @?= nested
  , testCase "nested sorting; keys . arrays" $ let f = sortKeys . sortArrays in
        f $(mkValue' "{'value': null, 'ints': [2,3,1], 'abc': {'b': 'y', 'c': 'z', 'a': 'x'}}") @?= nested
  , testCase "nested sorting; arrays and keys" $ let f = sortArraysAndKeys in
        f $(mkValue' "{'value': null, 'ints': [2,3,1], 'abc': {'b': 'y', 'c': 'z', 'a': 'x'}}") @?= nested
  ]

stripNulls :: Value -> Value
stripNulls = cata (embed . f) where
  f (ObjectF a) = ObjectF $ HM.filter (/= Null) a
  f x = x

sortArrays :: Value -> Value
sortArrays = cata (embed . f) where
  f (ArrayF xs) = ArrayF (V.fromList . sort $ V.toList xs)
  f x = x

sortKeys:: Value -> Value
sortKeys = cata (embed . f) where
  f (ObjectF a) = ObjectF (HM.fromList . sort $ HM.toList a)
  f x = x

sortArraysAndKeys:: Value -> Value
sortArraysAndKeys = cata (embed . f) where
  f (ObjectF a) = ObjectF (HM.fromList . sort $ HM.toList a)
  f (ArrayF xs) = ArrayF (V.fromList . sort $ V.toList xs)
  f x = x