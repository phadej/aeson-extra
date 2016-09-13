{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Main (main) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif



import           Data.Map (Map)
import           Data.Maybe (isJust)
import           Data.String (fromString)
import           Data.Time (zonedTimeToUTC, UTCTime(..), Day(..))
import           Data.Vector (Vector)
import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import Data.These (These (..))
import Data.Align (alignWith)

import qualified Data.HashMap.Lazy as H

#if MIN_VERSION_base(4,7,0)
import           Data.Proxy
#endif

import           Data.Aeson.Extra
import           Data.Time.TH

import           Orphans ()

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ dotColonMark
  , encodeStrictTests
  , mTests
#if MIN_VERSION_base(4,7,0)
  , symTests
  , singObjectTests
#endif
  , collapsedListTests
  , utctimeTests
  , zonedtimeTests
  , timeTHTests
  , mergeTests
  , streamTests
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
-- M
------------------------------------------------------------------------------

mTests :: TestTree
mTests = testGroup "M"
  [ testCase "decode" $ let lhs = decode "{\"1\": 1, \"2\": 2}" :: Maybe (M (H.HashMap Int Int))
                            rhs = Just result
                        in lhs @?= rhs
  , testProperty "decode . encode" $
      let prop :: Map Int Int -> Property
          prop m = let lhs = fmap getMap . decode . encode . M $ m
                       rhs = Just m
                   in lhs === rhs
      in prop
  ]
  where result = M $ H.fromList [(1,1),(2,2)]

#if MIN_VERSION_base(4,7,0)

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

#endif

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
-- U & Z
------------------------------------------------------------------------------

utctimeTests :: TestTree
utctimeTests = testGroup "U" $
  [ testCase "base case" $ assertBool "base case" $ isJust simple
  ] ++ map t timeStrings
  where simple = decode "\"2015-09-07T08:16:40.807Z\"" :: Maybe U
        t str = testCase str
              . assertEqual str simple
              . decode
              . fromString
              $ "\"" ++ str ++ "\""

zonedtimeTests :: TestTree
zonedtimeTests = testGroup "Z" $
  [ testCase "base case" $ assertBool "base case" $ isJust simple
  ] ++ map t timeStrings
  where simple = decode "\"2015-09-07T08:16:40.807Z\"" :: Maybe Z
        t str = testCase str
              . assertEqual str (fmap z simple)
              . fmap z
              . decode
              . fromString
              $ "\"" ++ str ++ "\""
        z (Z z') = zonedTimeToUTC z'

timeStrings :: [String]
timeStrings =
  [ "2015-09-07T08:16:40.807Z"
  , "2015-09-07T11:16:40.807+0300"
  , "2015-09-07 08:16:40.807Z"
  , "2015-09-07 08:16:40.807 Z"
  , "2015-09-07 08:16:40.807 +0000"
  , "2015-09-07 08:16:40.807 +00:00"
  , "2015-09-07 11:16:40.807 +03:00"
  , "2015-09-07 05:16:40.807 -03:00"
  ]

timeTHTests :: TestTree
timeTHTests =
    testCase "time TH example" $ assertBool "should be equal" $ lhs == rhs
      where lhs = UTCTime (ModifiedJulianDay 56789) 123.456
            rhs = $(mkUTCTime "2014-05-12 00:02:03.456000Z")

------------------------------------------------------------------------------
-- Merge tests
------------------------------------------------------------------------------

lodashMerge :: Value -> Value -> Value
lodashMerge x y = merge lodashMergeAlg x y

lodashMergeAlg :: (a -> a -> a) -> ValueF a -> ValueF a -> ValueF a
lodashMergeAlg r a' b' = case (a', b') of
    (ObjectF a, ObjectF b) -> ObjectF $ alignWith f a b
    (ArrayF a,  ArrayF b)  -> ArrayF $ alignWith f a b
    (_,         b)         -> b
  where f (These x y) = r x y
        f (This x)    = x
        f (That x)    = x

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
