module Data.HashPSQ.Tests
    ( tests
    ) where

import           Prelude                        hiding (lookup)

import           Test.Framework                       (Test)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck                      (Property, arbitrary,
                                                       forAll,
                                                       NonNegative(NonNegative))
import           Test.HUnit                           (Assertion, assert)

import           Data.HashPSQ.Internal
import qualified Data.OrdPSQ                    as OrdPSQ
import           Data.PSQ.Class.Gen
import           Data.PSQ.Class.Util


--------------------------------------------------------------------------------
-- Index of tests
--------------------------------------------------------------------------------

tests :: [Test]
tests =
    [ testCase      "showBucket"          test_showBucket
    , testCase      "toBucket"            test_toBucket
    , testProperty "unsafeLookupIncreasePriority"
                                          prop_unsafeLookupIncreasePriority
    , testProperty "unsafeInsertIncreasePriority"
                                          prop_unsafeInsertIncreasePriority
    , testProperty "unsafeInsertIncreasePriorityView"
                                          prop_unsafeInsertIncreasePriorityView
    , testProperty "takeMin_length"       prop_takeMin_length
    , testProperty "takeMin_increasing"   prop_takeMin_increasing
    , testProperty "insertWith_const"     prop_insertWith_const
    , testProperty "insertWith_flipconst" prop_insertWith_flipconst
    ]


--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

test_showBucket :: Assertion
test_showBucket =
    assert $ length (coverShowInstance bucket) > 0
  where
    bucket :: Bucket Int Int Char
    bucket = B 1 'a' OrdPSQ.empty

test_toBucket :: Assertion
test_toBucket =
    assert True
    -- TODO (jaspervdj)
    -- assert $ mkBucket (OrdPSQ.empty :: OrdPSQ.OrdPSQ Int Int Char)


--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

prop_unsafeLookupIncreasePriority :: Property
prop_unsafeLookupIncreasePriority =
    forAll arbitraryPSQ $ \t ->
    forAll arbitrary    $ \k  ->
        let newP       = maybe 0 ((+ 1) . fst) (lookup k t)
            (mbPx, t') = unsafeLookupIncreasePriority k newP t
            expect     = case mbPx of
                Nothing     -> Nothing
                Just (p, x) -> Just (p + 1, x)
        in valid (t' :: HashPSQ LousyHashedInt Int Char) &&
            lookup k t' == expect &&
            lookup k t  == mbPx

prop_unsafeInsertIncreasePriority :: Property
prop_unsafeInsertIncreasePriority =
    forAll arbitraryPSQ $ \t ->
    forAll arbitrary    $ \k ->
    forAll arbitrary    $ \x ->
        let prio = largerThanMaxPrio t
            t'   = unsafeInsertIncreasePriority k prio x t
        in valid (t' :: HashPSQ LousyHashedInt Int Char) &&
            lookup k t' == Just (prio, x)

prop_unsafeInsertIncreasePriorityView :: Property
prop_unsafeInsertIncreasePriorityView =
    forAll arbitraryPSQ $ \t ->
    forAll arbitrary    $ \k ->
    forAll arbitrary    $ \x ->
        let prio       = largerThanMaxPrio t
            (mbPx, t') = unsafeInsertIncreasePriorityView k prio x t
        in valid (t' :: HashPSQ LousyHashedInt Int Char) &&
            lookup k t' == Just (prio, x) &&
            lookup k t  == mbPx

prop_takeMin_length :: NonNegative Int -> HashPSQ Int Int Char -> Bool
prop_takeMin_length (NonNegative n) t = length (takeMin n t) <= n

prop_takeMin_increasing :: NonNegative Int -> HashPSQ Int Int Char -> Bool
prop_takeMin_increasing (NonNegative n) t = isSorted [p | (_, p, _) <- takeMin n t]
  where
    isSorted (x : y : zs) = x <= y && isSorted (y : zs)
    isSorted [_]          = True
    isSorted []           = True

prop_insertWith_const :: (Int,Int,Char) -> HashPSQ Int Int Char -> Bool
prop_insertWith_const (k,p,v) t = lookup k (i1 . i2 $ t) == Just (p + 1,succ v) where
  i1 = insertWith (const const) k (p + 1) (succ v)
  i2 = insert k p v

prop_insertWith_flipconst :: (Int,Int,Char) -> HashPSQ Int Int Char -> Bool
prop_insertWith_flipconst (k,p,v) t = lookup k (i1 . i2 $ t) == Just (p,v) where
  i1 = insertWith (const $ flip const) k (p + 1) (succ v)
  i2 = insert k p v
