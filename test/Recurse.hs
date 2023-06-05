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
empty = $(mkValue' "{}")

sortedInts :: Value
sortedInts = $(mkValue' "{\"ints\": [1,2,3]}")

recurseTests :: TestTree
recurseTests = testGroup "Recurse examples"
  [ testCase "strip nulls" $ stripNulls $(mkValue' "{\"value\": null}") @?= empty
  , testCase "sort arrays" $ sortArrays $(mkValue' "{\"ints\": [2,1,3]}") @?= sortedInts
  ]

stripNulls :: Value -> Value
stripNulls = cata (embed . f) where
  f (ObjectF a) = ObjectF $ HM.filter (/= Null) a
  f x = x

sortArrays :: Value -> Value
sortArrays = cata (embed . f) where
  f (ArrayF xs) = ArrayF (V.fromList . sort $ V.toList xs)
  f x = x