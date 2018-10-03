{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Validation.Verifiers.NumbersSpec where

import Test.Hspec
import Data.Proxy
import Data.Validation

type LT4_5 = $(lt 4.5) Double
type LTE4_5 = $(lte 4.5) Double
type GT4_5 = $(gt 4.5) Double
type GTE4_5 = $(gte 4.5) Double

type Inside0_5 = $(inside 0 5 False False) Double
type InsideE0_5 = $(inside 0 5 True True) Double
type Outside0_5 = $(outside 0 5 False False) Double
type OutsideE0_5 = $(outside 0 5 True True) Double

spec :: Spec
spec = do
    let vspec v t = let (n, a) = verifierSpec v in (n, t <-$ a)

    describe "Verifier definitions" $ do
        it "Less-than of real value" $ do
            vspec (Proxy :: Proxy LT4_5) id `shouldBe` ("real.lt", 4.5)
        it "Less-than-or-equal-to of real value" $ do
            vspec (Proxy :: Proxy LTE4_5) id `shouldBe` ("real.lte", 4.5)
        it "Greater-than of real value" $ do
            vspec (Proxy :: Proxy GT4_5) id `shouldBe` ("real.gt", 4.5)
        it "Greater-than-or-equal-to of real value" $ do
            vspec (Proxy :: Proxy GTE4_5) id `shouldBe` ("real.gte", 4.5)
        it "Inside-excluding-endpoints of real value" $ do
            vspec (Proxy :: Proxy Inside0_5) (,) `shouldBe` ("real.gt.lt", (0, 5))
        it "Inside-including-endpoints of real value" $ do
            vspec (Proxy :: Proxy InsideE0_5) (,) `shouldBe` ("real.gte.lte", (0, 5))
        it "Outside-excluding-endpoints of real value" $ do
            vspec (Proxy :: Proxy Outside0_5) (,) `shouldBe` ("real.lt.gt", (0, 5))
        it "Outside-including-endpoints of real value" $ do
            vspec (Proxy :: Proxy OutsideE0_5) (,) `shouldBe` ("real.lte.gte", (0, 5))

        it "Less-than of integral value" $ do
            vspec (Proxy :: Proxy (LT Int 5)) id `shouldBe` ("int.lt", 5)
        it "Less-than-or-equal-to of integral value" $ do
            vspec (Proxy :: Proxy (LTE Int 5)) id `shouldBe` ("int.lte", 5)
        it "Greater-than of integral value" $ do
            vspec (Proxy :: Proxy (GT Int 5)) id `shouldBe` ("int.gt", 5)
        it "Greater-than-or-equal-to of integral value" $ do
            vspec (Proxy :: Proxy (GTE Int 5)) id `shouldBe` ("int.gte", 5)
        it "Inside of integral value" $ do
            vspec (Proxy :: Proxy (InRange Int 3 5)) (,) `shouldBe` ("int.inRange", (3, 5))
        it "Outside of integral value" $ do
            vspec (Proxy :: Proxy (OutOfRange Int 3 5)) (,) `shouldBe` ("int.outOfRange", (3, 5))

    describe "CompReal" $ do
        it "Valid lt" $ do
            verify (Proxy :: Proxy LT4_5) 4 `shouldBe` Right 4
        it "Equality is invalid in lt" $ do
            verify (Proxy :: Proxy LT4_5) 4.5 `shouldBe` Left ()
        it "Invalid lt" $ do
            verify (Proxy :: Proxy LT4_5) 5 `shouldBe` Left ()

        it "Valid lte" $ do
            verify (Proxy :: Proxy LTE4_5) 4 `shouldBe` Right 4
        it "Equality is valid in lte" $ do
            verify (Proxy :: Proxy LTE4_5) 4.5 `shouldBe` Right 4.5
        it "Invalid lte" $ do
            verify (Proxy :: Proxy LTE4_5) 5 `shouldBe` Left ()

        it "Valid gt" $ do
            verify (Proxy :: Proxy GT4_5) 5 `shouldBe` Right 5
        it "Equality is invalid in gt" $ do
            verify (Proxy :: Proxy GT4_5) 4.5 `shouldBe` Left ()
        it "Invalid gt" $ do
            verify (Proxy :: Proxy GT4_5) 4 `shouldBe` Left ()

        it "Valid gte" $ do
            verify (Proxy :: Proxy GTE4_5) 5 `shouldBe` Right 5
        it "Equality is valid in gte" $ do
            verify (Proxy :: Proxy GTE4_5) 4.5 `shouldBe` Right 4.5
        it "Invalid gte" $ do
            verify (Proxy :: Proxy GTE4_5) 4 `shouldBe` Left ()

    describe "RangeReal" $ do
        it "Smaller than (0, 5)" $ do
            verify (Proxy :: Proxy Inside0_5) (-0.1) `shouldBe` Left ()
        it "Bottom equality is invalid" $ do
            verify (Proxy :: Proxy Inside0_5) 0 `shouldBe` Left ()
        it "Valid in (0, 5)" $ do
            verify (Proxy :: Proxy Inside0_5) 0.1 `shouldBe` Right 0.1
        it "Top equality is invalid" $ do
            verify (Proxy :: Proxy Inside0_5) 5 `shouldBe` Left ()
        it "Larger than (0, 5)" $ do
            verify (Proxy :: Proxy Inside0_5) 5.1 `shouldBe` Left ()

        it "Smaller than [0, 5]" $ do
            verify (Proxy :: Proxy InsideE0_5) (-0.1) `shouldBe` Left ()
        it "Bottom equality is valid" $ do
            verify (Proxy :: Proxy InsideE0_5) 0 `shouldBe` Right 0
        it "Valid in [0, 5]" $ do
            verify (Proxy :: Proxy InsideE0_5) 0.1 `shouldBe` Right 0.1
        it "Top equality is valid" $ do
            verify (Proxy :: Proxy InsideE0_5) 5 `shouldBe` Right 5
        it "Larger than [0, 5]" $ do
            verify (Proxy :: Proxy InsideE0_5) 5.1 `shouldBe` Left ()

        it "Left side of (0, 5)" $ do
            verify (Proxy :: Proxy Outside0_5) (-0.1) `shouldBe` Right (-0.1)
        it "Bottom equality is invalid" $ do
            verify (Proxy :: Proxy Outside0_5) 0 `shouldBe` Left ()
        it "Included in (0, 5)" $ do
            verify (Proxy :: Proxy Outside0_5) 0.1 `shouldBe` Left ()
        it "Top equality is invalid" $ do
            verify (Proxy :: Proxy Outside0_5) 5 `shouldBe` Left ()
        it "Right side of (0, 5)" $ do
            verify (Proxy :: Proxy Outside0_5) 5.1 `shouldBe` Right 5.1

        it "Left side of [0, 5]" $ do
            verify (Proxy :: Proxy OutsideE0_5) (-0.1) `shouldBe` Right (-0.1)
        it "Bottom equality is valid" $ do
            verify (Proxy :: Proxy OutsideE0_5) 0 `shouldBe` Right 0
        it "Included in [0, 5]" $ do
            verify (Proxy :: Proxy OutsideE0_5) 0.1 `shouldBe` Left ()
        it "Top equality is valid" $ do
            verify (Proxy :: Proxy OutsideE0_5) 5 `shouldBe` Right 5
        it "Right side of [0, 5]" $ do
            verify (Proxy :: Proxy OutsideE0_5) 5.1 `shouldBe` Right 5.1

    describe "Comp" $ do
        it "Valid for less-than" $ do
            verify (Proxy :: Proxy (LT Int 5)) 4 `shouldBe` Right 4
        it "Equality is invalid for less-than" $ do
            verify (Proxy :: Proxy (LT Int 5)) 5 `shouldBe` Left ()
        it "Invalid for less-than" $ do
            verify (Proxy :: Proxy (LT Int 5)) 6 `shouldBe` Left ()

        it "Valid for less-than-or-equal-to" $ do
            verify (Proxy :: Proxy (LTE Int 5)) 4 `shouldBe` Right 4
        it "Equality is valid for less-than-or-equal-to" $ do
            verify (Proxy :: Proxy (LTE Int 5)) 5 `shouldBe` Right 5
        it "Invalid for less-than-or-equal-to" $ do
            verify (Proxy :: Proxy (LTE Int 5)) 6 `shouldBe` Left ()

        it "Valid for greater-than" $ do
            verify (Proxy :: Proxy (GT Int 5)) 4 `shouldBe` Left ()
        it "Equality is invalid for greater-than" $ do
            verify (Proxy :: Proxy (GT Int 5)) 5 `shouldBe` Left ()
        it "Invalid for greater-than" $ do
            verify (Proxy :: Proxy (GT Int 5)) 6 `shouldBe` Right 6

        it "Valid for greater-than-or-equal-to" $ do
            verify (Proxy :: Proxy (GTE Int 5)) 4 `shouldBe` Left ()
        it "Equality is valid for greater-than-or-equal-to" $ do
            verify (Proxy :: Proxy (GTE Int 5)) 5 `shouldBe` Right 5
        it "Invalid for greater-than-or-equal-to" $ do
            verify (Proxy :: Proxy (GTE Int 5)) 6 `shouldBe` Right 6

    describe "Range" $ do
        it "Left side of range" $ do
            verify (Proxy :: Proxy (InRange Int 3 5)) 2 `shouldBe` Left ()
        it "Left equality is valid for in-range" $ do
            verify (Proxy :: Proxy (InRange Int 3 5)) 3 `shouldBe` Right 3
        it "Valid for in-range" $ do
            verify (Proxy :: Proxy (InRange Int 3 5)) 4 `shouldBe` Right 4
        it "Right equality is valid for in-range" $ do
            verify (Proxy :: Proxy (InRange Int 3 5)) 5 `shouldBe` Right 5
        it "Right side of range" $ do
            verify (Proxy :: Proxy (InRange Int 3 5)) 6 `shouldBe` Left ()

        it "Left side of range" $ do
            verify (Proxy :: Proxy (OutOfRange Int 3 5)) 2 `shouldBe` Right 2
        it "Left equality is invalid for out-of-range" $ do
            verify (Proxy :: Proxy (OutOfRange Int 3 5)) 3 `shouldBe` Left ()
        it "Valid for in-range" $ do
            verify (Proxy :: Proxy (OutOfRange Int 3 5)) 4 `shouldBe` Left ()
        it "Right equality is invalid for out-of-range" $ do
            verify (Proxy :: Proxy (OutOfRange Int 3 5)) 5 `shouldBe` Left ()
        it "Right side of range" $ do
            verify (Proxy :: Proxy (OutOfRange Int 3 5)) 6 `shouldBe` Right 6