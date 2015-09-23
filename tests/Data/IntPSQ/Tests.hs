module Data.IntPSQ.Tests
    where

import           Prelude hiding (lookup)

import           Test.QuickCheck                      (Property, arbitrary,
                                                       forAll)
import           Test.Framework                       (Test)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit                           (Assertion, assert)
import           Test.QuickCheck                      (NonNegative(NonNegative))

import           Data.IntPSQ.Internal
import           Data.PSQ.Class.Gen
import           Data.PSQ.Class.Util

--------------------------------------------------------------------------------
-- Index of tests
--------------------------------------------------------------------------------

tests :: [Test]
tests =
    [ testCase     "hasBadNils"           test_hasBadNils
    , testProperty "unsafeInsertIncreasePriority"
                                          prop_unsafeInsertIncreasePriority
    , testProperty "unsafeInsertIncreasePriorityView"
                                          prop_unsafeInsertIncreasePriorityView
    , testProperty "unsafeInsertWithIncreasePriority"
                                          prop_unsafeInsertWithIncreasePriority
    , testProperty "unsafeInsertWithIncreasePriorityView"
                                          prop_unsafeInsertWithIncreasePriorityView
    , testProperty "unsafeLookupIncreasePriority"
                                          prop_unsafeLookupIncreasePriority
    , testProperty "takeMin_length"       prop_takeMin_length
    , testProperty "takeMin_increasing"   prop_takeMin_increasing
    , testProperty "insertWith_const"     prop_insertWith_const
    , testProperty "insertWith_flipconst" prop_insertWith_flipconst
    ]


--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

-- 100% test coverage...
test_hasBadNils :: Assertion
test_hasBadNils =
    assert $ hasBadNils (Bin 1 (2 :: Int) 'x' 0 Nil Nil)


--------------------------------------------------------------------------------
-- QuickCheck properties
--------------------------------------------------------------------------------

prop_unsafeInsertIncreasePriority :: Property
prop_unsafeInsertIncreasePriority =
    forAll arbitraryPSQ $ \t ->
    forAll arbitrary    $ \k ->
    forAll arbitrary    $ \x ->
        let prio = largerThanMaxPrio t
            t'   = unsafeInsertIncreasePriority k prio x t
        in valid (t' :: IntPSQ Int Char) && lookup k t' == Just (prio, x)

prop_unsafeInsertIncreasePriorityView :: Property
prop_unsafeInsertIncreasePriorityView =
    forAll arbitraryPSQ $ \t ->
    forAll arbitrary    $ \k ->
    forAll arbitrary    $ \x ->
        let prio       = largerThanMaxPrio t
            (mbPx, t') = unsafeInsertIncreasePriorityView k prio x t
        in valid (t' :: IntPSQ Int Char) &&
            lookup k t' == Just (prio, x) &&
            lookup k t  == mbPx

prop_unsafeInsertWithIncreasePriority :: Property
prop_unsafeInsertWithIncreasePriority =
    forAll arbitraryPSQ $ \t0 ->
    forAll arbitrary    $ \k  ->
    forAll arbitrary    $ \x  ->
        let t      = fmap (\e -> [e]) t0 :: IntPSQ Int [Char]
            prio   = largerThanMaxPrio t
            f      = \newP newX oldP oldX ->
                        (min newP oldP + 1, newX ++ oldX)
            t'     = unsafeInsertWithIncreasePriority f k prio [x] t
            expect = case lookup k t of
                            Nothing     -> (prio, [x])
                            Just (p, y) -> (min prio p + 1, [x] ++ y)
        in valid t' && lookup k t' == Just expect

prop_unsafeInsertWithIncreasePriorityView :: Property
prop_unsafeInsertWithIncreasePriorityView =
    forAll arbitraryPSQ $ \t0 ->
    forAll arbitrary    $ \k  ->
    forAll arbitrary    $ \x  ->
        let t          = fmap (\e -> [e]) t0 :: IntPSQ Int [Char]
            prio       = largerThanMaxPrio t
            f          = \newP newX oldP oldX ->
                            (min newP oldP + 1, newX ++ oldX)
            (mbPx, t') = unsafeInsertWithIncreasePriorityView f k prio [x] t
            expect     = case mbPx of
                            Nothing     -> (prio, [x])
                            Just (p, y) -> (min prio p + 1, [x] ++ y)
        in valid t' &&
            lookup k t' == Just expect &&
            lookup k t  == mbPx

prop_unsafeLookupIncreasePriority :: Property
prop_unsafeLookupIncreasePriority =
    forAll arbitraryPSQ $ \t0 ->
    forAll arbitrary    $ \k  ->
        let t          = fmap (\e -> [e]) t0 :: IntPSQ Int [Char]
            f          = \oldP oldX ->
                            (Just (oldP, oldX), oldP + 1, oldX ++ "k")
            (mbPx, t') = unsafeLookupIncreasePriority f k t
            expect     = case mbPx of
                Nothing     -> Nothing
                Just (p, x) -> Just (p + 1, x ++ "k")
        in valid t' &&
            lookup k t' == expect &&
            lookup k t  == mbPx

prop_takeMin_length :: NonNegative Int -> IntPSQ Int Char -> Bool
prop_takeMin_length (NonNegative n) t = length (takeMin n t) <= n

prop_takeMin_increasing :: NonNegative Int -> IntPSQ Int Char -> Bool
prop_takeMin_increasing (NonNegative n) t = isSorted [p | (_, p, _) <- takeMin n t]
  where
    isSorted (x : y : zs) = x <= y && isSorted (y : zs)
    isSorted [_]          = True
    isSorted []           = True

prop_insertWith_const :: (Int,Int,Char) -> IntPSQ Int Char -> Bool
prop_insertWith_const (k,p,v) t = lookup k (i1 . i2 $ t) == Just (p + 1,succ v) where
  i1 = insertWith (const const) k (p + 1) (succ v)
  i2 = insert k p v

prop_insertWith_flipconst :: (Int,Int,Char) -> IntPSQ Int Char -> Bool
prop_insertWith_flipconst (k,p,v) t = lookup k (i1 . i2 $ t) == Just (p,v) where
  i1 = insertWith (const $ flip const) k (p + 1) (succ v)
  i2 = insert k p v
