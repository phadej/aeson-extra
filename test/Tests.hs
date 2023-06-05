{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Main (main) where

import Prelude ()
import Prelude.Compat

import Data.Aeson
import Data.Aeson.Extra

import Data.Maybe                (isJust)
import Data.Proxy
import Data.Vector               (Vector)
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Orphans ()
import Recurse (recurseTests)

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ dotColonMark
  , encodeStrictTests
  , symTests
  , singObjectTests
  , collapsedListTests
  , mergeTests
  , streamTests
  , recurseTests
  ]

------------------------------------------------------------------------------
-- encodeStrict
------------------------------------------------------------------------------
encodeStrictTests :: TestTree
encodeStrictTests = testGroup "encodeStrict"
  [ testProperty "decodeStrict . encodeStrict" prop
  ]
  where prop :: Int -> Property
        prop i = let lhs = decodeStrict . encodeStrict $ i
                     rhs = Just i
                 in lhs === rhs

------------------------------------------------------------------------------
-- SymTag
------------------------------------------------------------------------------

symTests :: TestTree
symTests = testGroup "SymTag"
  [ testCase "encode" $ encode (SymTag :: SymTag "foobar") @?= "\"foobar\""
  , testCase "decode success" $ (decode "\"foobar\"" :: Maybe (SymTag "foobar")) @?= Just SymTag
  , testCase "decode failure" $ (decode "\"foobar\"" :: Maybe (SymTag "barfoo")) @?= Nothing
  ]

------------------------------------------------------------------------------
-- SingObject
------------------------------------------------------------------------------

-- > λ > decode "{\"value\": 42 }" :: Maybe (SingObject "value" Int)
-- > Just (SingObject 42)

singObjectTests :: TestTree
singObjectTests = testGroup "SingObject"
  [ testCase "decode success" $ (decode "{\"value\": 42 }" :: Maybe (SingObject "value" Int)) @?= Just (SingObject 42)
  , testCase "decode failure" $ (decode "{\"value\": 42 }" :: Maybe (SingObject "key" Int)) @?= Nothing
  , testProperty "decode . encode" $
      let prop :: Int -> Property
          prop n = let rhs = fmap (getSingObject p) . decode . encode . mkSingObject p $ n
                       lhs = Just n
                   in lhs === rhs
          p :: Proxy "value"
          p = Proxy
      in prop
  ]

------------------------------------------------------------------------------
-- parseCollapsedList
------------------------------------------------------------------------------

newtype V = V [Int] deriving (Show, Eq)
instance FromJSON V where parseJSON = withObject "V" $ \obj -> V <$> parseCollapsedList obj "value"

collapsedListTests :: TestTree
collapsedListTests = testGroup "collapsedList"
  [ testCase "empty"     $ (decode "{}" :: Maybe V) @?= Just (V [])
  , testCase "null"      $ (decode "{\"value\": null}" :: Maybe V) @?= Just (V [])
  , testCase "singleton" $ (decode "{\"value\": 42}" :: Maybe V) @?= Just (V [42])
  , testCase "array"     $ (decode "{\"value\": [1, 2, 3, 4]}" :: Maybe V) @?= Just (V [1,2,3,4])
  , testProperty "decode . encode" $
      let prop :: [Int] -> Property
          prop l = let rhs = fmap getCollapsedList . decode . encode . CollapsedList $ l
                       lhs = Just l
                   in lhs === rhs
      in prop
  , testProperty "Vector decode . encode" $
      let prop :: Vector Int -> Property
          prop l = let rhs = fmap getCollapsedList . decode . encode . CollapsedList $ l
                       lhs = Just l
                   in lhs === rhs
      in prop
  ]

-------------------------------------------------------------------------------
-- Stream
-------------------------------------------------------------------------------

streamTests :: TestTree
streamTests = testGroup "stream"
    [ streamDecodeTests
    ]
  where
    streamDecodeTests = testGroup "decode" $
        map (uncurry validTestCase) valids ++
        [ testCase "ws: empty"     $ streamDecode " [ ] "           @?= ([]      :: [Int], Nothing)
        , testCase "ws: singleton" $ streamDecode " [ 1 ]"          @?= ([1]     :: [Int], Nothing)
        , testCase "ws: many"      $ streamDecode " [ 1 , 2, 3 ]  " @?= ([1,2,3] :: [Int], Nothing)
        -- Errors:
        , testCase "error begin"   $ streamDecode' ","         @?= ([]    :: [Int], True)
        , testCase "parses first"  $ streamDecode' "[1,2,3["   @?= ([1,2] :: [Int], True)
        , testCase "error begin"   $ streamDecode' "[1,2,'a']" @?= ([1,2] :: [Int], True)
        ]

    validTestCase name v =
        testCase ("valid " ++ name) $ streamDecode (encode v) @?= (v, Nothing)

    streamDecode' = fmap isJust . streamDecode

    valids :: [(String, [Int])]
    valids =
        [ (,) "empty"     []
        , (,) "singleton" [1]
        , (,) "many"      [1..200]
        ]


------------------------------------------------------------------------------
-- Comparison (.:?) and (.:!)
------------------------------------------------------------------------------

newtype T1 = T1 (Maybe Int) deriving (Eq, Show)
newtype T2 = T2 (Maybe Int) deriving (Eq, Show)
newtype T3 = T3 (Maybe Int) deriving (Eq, Show)

instance FromJSON T1 where parseJSON = fmap T1 . withObject "T1" (.: "value")
instance FromJSON T2 where parseJSON = fmap T2 . withObject "T2" (.:? "value")
instance FromJSON T3 where parseJSON = fmap T3 . withObject "T3" (.:! "value")

dotColonMark :: TestTree
dotColonMark = testGroup "Operators" $ fmap t [
    assertEqual ".:  not-present" Nothing               (decode ex1 :: Maybe T1)
  , assertEqual ".:  42"          (Just (T1 (Just 42))) (decode ex2 :: Maybe T1)
  , assertEqual ".:  null"        (Just (T1 Nothing))   (decode ex3 :: Maybe T1)

  , assertEqual ".:? not-present" (Just (T2 (Nothing))) (decode ex1 :: Maybe T2)
  , assertEqual ".:? 42"          (Just (T2 (Just 42))) (decode ex2 :: Maybe T2)
  , assertEqual ".:? null"        (Just (T2 Nothing))   (decode ex3 :: Maybe T2)

  , assertEqual ".:! not-present" (Just (T3 (Nothing))) (decode ex1 :: Maybe T3)
  , assertEqual ".:! 42"          (Just (T3 (Just 42))) (decode ex2 :: Maybe T3)
  , assertEqual ".:! null"        Nothing               (decode ex3 :: Maybe T3)
  ]
  where ex1 = "{}"
        ex2 = "{\"value\": 42 }"
        ex3 = "{\"value\": null }"
        t   = testCase "-"

------------------------------------------------------------------------------
-- Merge tests
------------------------------------------------------------------------------

mergeTests :: TestTree
mergeTests = testGroup "Lodash merge examples" $ map f examples
  where
    f (x, y, z) = testCase "-" $ assertBool "should be equal" $ lodashMerge x y == z
    examples =
      [ (,,) $(mkValue "[1, 2, 3]") $(mkValue "[4, 5, 6, 7, 8]") $(mkValue "[4, 5, 6, 7, 8]")
      , (,,) $(mkValue' "{'a': 1}") $(mkValue' "{'b': 2}") $(mkValue' "{'a': 1, 'b': 2}")
      , (,,)
        $(mkValue' "{ 'data': [{ 'user': 'barney' }, { 'user': 'fred' }] }")
        $(mkValue' "{ 'data': [{ 'age': 36 }, { 'age': 40 }] }")
        $(mkValue' "{ 'data': [{ 'user': 'barney', 'age': 36 }, { 'user': 'fred', 'age': 40 }] }")
      ]
