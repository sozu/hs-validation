{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Validation.Verifiers.StringsSpec where

import Test.Hspec
import Data.Proxy
import Data.Char
import Data.Validation

$(declareCharFilter "IsAlpha" "alphabet" 'isAlpha)

type Len3_5 = Length String 3 5
type Longer3 = Longer String 3
type LongerOr3 = LongerOr String 3
type Shorter5 = Shorter String 5
type ShorterOr5 = ShorterOr String 5
type Alphabet = CharOf IsAlpha

spec :: Spec
spec = do
    let vspec v t = let (n, a) = verifierSpec v in (n, t <-$ a)

    describe "Verifier definitions" $ do
        it "length verifier" $ do
            vspec (Proxy :: Proxy Len3_5) (,) `shouldBe` ("str.length", (3, 5))
        it "longer verifier" $ do
            vspec (Proxy :: Proxy Longer3) id `shouldBe` ("str.longer", 3)
        it "longer-or-equal-to verifier" $ do
            vspec (Proxy :: Proxy LongerOr3) id `shouldBe` ("str.longerOr", 3)
        it "shorter verifier" $ do
            vspec (Proxy :: Proxy Shorter5) id `shouldBe` ("str.shorter", 5)
        it "shorter-or-equal-to verifier" $ do
            vspec (Proxy :: Proxy ShorterOr5) id `shouldBe` ("str.shorterOr", 5)
        it "char-of verifier" $ do
            vspec (Proxy :: Proxy Alphabet) id `shouldBe` ("str.charOf.alphabet", "alphabet")

    describe "Length" $ do
        it "Too short length out of range" $ do
            verify (Proxy :: Proxy Len3_5) "ab" `shouldBe` Left ()
        it "Bottom of range is valid" $ do
            verify (Proxy :: Proxy Len3_5) "abc" `shouldBe` Right "abc"
        it "Valid length" $ do
            verify (Proxy :: Proxy Len3_5) "abcd" `shouldBe` Right "abcd"
        it "Top of range is valid" $ do
            verify (Proxy :: Proxy Len3_5) "abcde" `shouldBe` Right "abcde"
        it "Too long length out of range" $ do
            verify (Proxy :: Proxy Len3_5) "abcdef" `shouldBe` Left ()

    describe "Longer" $ do
        it "Shorter than threshold" $ do
            verify (Proxy :: Proxy Longer3) "ab" `shouldBe` Left ()
        it "Equality to bottom is invalid" $ do
            verify (Proxy :: Proxy Longer3) "abc" `shouldBe` Left ()
        it "Valid length" $ do
            verify (Proxy :: Proxy Longer3) "abcd" `shouldBe` Right "abcd"

    describe "LongerOr" $ do
        it "Shorter than threshold" $ do
            verify (Proxy :: Proxy LongerOr3) "ab" `shouldBe` Left ()
        it "Equality to bottom is valid" $ do
            verify (Proxy :: Proxy LongerOr3) "abc" `shouldBe` Right "abc"
        it "Valid length" $ do
            verify (Proxy :: Proxy LongerOr3) "abcd" `shouldBe` Right "abcd"

    describe "Shorter" $ do
        it "Valid length" $ do
            verify (Proxy :: Proxy Shorter5) "abcd" `shouldBe` Right "abcd"
        it "Equality to top is invalid" $ do
            verify (Proxy :: Proxy Shorter5) "abcde" `shouldBe` Left ()
        it "Longer than threshold" $ do
            verify (Proxy :: Proxy Shorter5) "abcdef" `shouldBe` Left ()

    describe "ShorterOr" $ do
        it "Valid length" $ do
            verify (Proxy :: Proxy ShorterOr5) "abcd" `shouldBe` Right "abcd"
        it "Equality to top is valid" $ do
            verify (Proxy :: Proxy ShorterOr5) "abcde" `shouldBe` Right "abcde"
        it "Longer than threshold" $ do
            verify (Proxy :: Proxy ShorterOr5) "abcdef" `shouldBe` Left ()

    describe "Character filter selecting alphabet" $ do
        it "Alphabetic characters" $ do
            verify (Proxy :: Proxy Alphabet) "abc" `shouldBe` Right "abc"
        it "Including non-alphabetic character" $ do
            verify (Proxy :: Proxy Alphabet) "a0c" `shouldBe` Left ()