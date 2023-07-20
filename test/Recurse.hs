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
import Data.List (sort, sortOn)
import Data.Text (Text)

import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Ord (Down(..))

empty :: Value
empty = $(mkValue "{}")

sortedInts :: Value
sortedInts = $(mkValue' "{'ints': [1,2,3]}")

sortedDownInts :: Value
sortedDownInts = $(mkValue' "{'ints': [3,2,1]}")

sortedKeys :: Value
sortedKeys = $(mkValue' "{'a': 'x', 'b': 'y', 'c': 'z'}")

sortedDownKeys :: Value
sortedDownKeys = $(mkValue' "{'c': 'z', 'b': 'y', 'a': 'x'}")

combined :: Value
combined = $(mkValue' "{'b': 'y', 'c': [1,2,3]}")

nested :: Value
nested = $(mkValue' "{'abc': {'a': 'x', 'b': 'y', 'c': 'z'}, 'ints': [1,2,3], 'value': null}")

nestedDown :: Value
nestedDown = $(mkValue' "{'value': null, 'ints': [3,2,1], 'abc': {'c': 'z', 'b': 'y', 'a': 'x'}}")

nestedDownKeysUpArrays :: Value
nestedDownKeysUpArrays = $(mkValue' "{'value': null, 'ints': [1,2,3], 'abc': {'c': 'z', 'b': 'y', 'a': 'x'}}")

nestedUpKeysDownArrays :: Value
nestedUpKeysDownArrays = $(mkValue' "{'abc': {'a': 'x', 'b': 'y', 'c': 'z'}, 'ints': [3,2,1], 'value': null}")

bca :: Value
bca = $(mkValue' "{'b': 'y', 'c': [2,3,1], 'a': null }")

vio :: Value
vio = $(mkValue' "{'value': null, 'ints': [2,3,1], 'abc': {'b': 'y', 'c': 'z', 'a': 'x'}}")

recurseTests :: TestTree
recurseTests = testGroup "Recurse examples"
  [ testCase "strip nulls" $ stripNulls $(mkValue' "{'value': null}") @?= empty
  , testGroup "Strip and Sort"
      [ testCase "strip nulls . sort arrays . sort keys" $ (stripNulls . sortArrays . sortKeys) bca @?= combined
      , testCase "strip nulls . sort keys . sort arrays" $ (stripNulls . sortKeys . sortArrays) bca @?= combined
      , testCase "sort keys . strip nulls . sort arrays" $ (sortKeys . stripNulls . sortArrays) bca @?= combined
      , testCase "sort keys . sort arrays . strip nulls" $ (sortKeys . sortArrays . stripNulls) bca @?= combined
      , testCase "sort arrays . sort keys . strip nulls" $ (sortArrays . sortKeys . stripNulls) bca @?= combined
      , testCase "sort arrays . strip nulls . sort keys" $ (sortArrays . stripNulls . sortKeys) bca @?= combined
      ]
  , testGroup "Sort Up"
      [ testCase "sort arrays" $ sortArrays $(mkValue' "{'ints': [2,1,3]}") @?= sortedInts
      , testCase "sort keys" $ sortKeys $(mkValue' "{'b': 'y', 'c': 'z', 'a': 'x'}") @?= sortedKeys
      , testCase "nested sorting; arrays . keys" $ (sortArrays . sortKeys) vio @?= nested
      , testCase "nested sorting; keys . arrays" $ (sortKeys . sortArrays) vio @?= nested
      , testCase "nested sorting; arrays and keys" $ sortArraysAndKeys vio @?= nested
      ]
  , testGroup "Sort Down"
      [ testCase "sort down arrays" $ sortDownArrays $(mkValue' "{'ints': [2,1,3]}") @?= sortedDownInts
      , testCase "sort down keys" $ sortDownKeys $(mkValue' "{'b': 'y', 'c': 'z', 'a': 'x'}") @?= sortedDownKeys
      , testCase "nested down sorting; arrays . keys" $ (sortDownArrays . sortDownKeys) vio @?= nestedDown
      , testCase "nested down sorting; keys . arrays" $ (sortDownKeys . sortDownArrays) vio @?= nestedDown
      , testCase "nested down sorting; arrays and keys" $ sortDownArraysAndKeys vio @?= nestedDown
      ]
  , testGroup "Sort Up Down and Down Up"
      [ testCase "down sorting arrays . up sorting keys" $ (sortDownArrays . sortKeys) vio @?= nestedUpKeysDownArrays
      , testCase "up sorting arrays . down sorting keys" $ (sortArrays . sortDownKeys) vio @?= nestedDownKeysUpArrays
      ]
  ]

transformArrays :: ([Value] -> [Value]) -> Value -> Value
transformArrays t = cata (embed . f) where
  f (ArrayF xs) = ArrayF (V.fromList . t $ V.toList xs)
  f x = x

transformKeys:: ([(Text, Value)] -> [(Text, Value)]) -> Value -> Value
transformKeys t = cata (embed . f) where
  f (ObjectF a) = ObjectF (HM.fromList . t $ HM.toList a)
  f x = x

transform :: ([(Text, Value)] -> [(Text, Value)]) -> ([Value] -> [Value]) -> Value -> Value
transform to ta = cata (embed . f) where
  f (ObjectF a) = ObjectF (HM.fromList . to $ HM.toList a)
  f (ArrayF xs) = ArrayF (V.fromList . ta $ V.toList xs)
  f x = x

stripNulls, sortArrays, sortDownArrays, sortKeys, sortDownKeys, sortArraysAndKeys, sortDownArraysAndKeys :: Value -> Value

stripNulls = cata (embed . f) where
  f (ObjectF a) = ObjectF $ HM.filter (/= Null) a
  f x = x

sortArrays = transformArrays sort
sortDownArrays = transformArrays $ sortOn Down
sortKeys = transformKeys sort
sortDownKeys = transformKeys $ sortOn Down
sortArraysAndKeys = transform sort sort
sortDownArraysAndKeys = transform (sortOn Down) (sortOn Down)