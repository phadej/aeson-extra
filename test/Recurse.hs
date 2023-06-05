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

import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.HashMap.Strict as HM

empty :: Value
empty = $(mkValue' "{}")

recurseTests :: TestTree
recurseTests = testGroup "Recurse examples"
  [ testCase "strip nulls" $ stripNulls $(mkValue' "{\"value\": null}") @?= empty
  ]

stripNulls :: Value -> Value
stripNulls = cata (embed . f) where
  f (ObjectF a) = ObjectF $ HM.filter (/= Null) a
  f x = x