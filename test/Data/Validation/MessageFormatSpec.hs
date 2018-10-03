{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Validation.MessageFormatSpec where

import Test.Hspec
import Data.Proxy
import qualified Data.Map as M
import Data.Validation.Validation

data TestVerifier = TestVerifier

instance Verifier TestVerifier where
    type VerifiableType TestVerifier = String
    type VerifierSpec TestVerifier = '[Int, String]
    type FailureHint TestVerifier = Double
    verifierSpec _ = ("test", 1 `ACons` "xyz" `ACons` ANil)
    verify _ v = Right v

spec :: Spec
spec = do
    describe "Read format of error message" $ do
        it "No variables" $ do
            readErrorFormat "" "abc" [] `shouldBe` ("abc", [])

        it "%% is not a variable" $ do
            readErrorFormat "" "%%abc" [] `shouldBe` ("%%abc", [])
            readErrorFormat "" "a%%bc" [] `shouldBe` ("a%%bc", [])
            readErrorFormat "" "abc%%" [] `shouldBe` ("abc%%", [])

        it "Unindexed variables" $ do
            readErrorFormat "" "%dabc%d" [] `shouldBe` ("%dabc%d", [0, 1])

        it "Indexed variables" $ do
            readErrorFormat "" "%3$dabc%1$d" [] `shouldBe` ("%dabc%d", [3, 1])

        it "0 index is available" $ do
            readErrorFormat "" "%0$dabc%d" [] `shouldBe` ("%dabc%d", [0, 1])

        it "Index is determined by incrementing previous index" $ do
            readErrorFormat "" "%3$dabc%d" [] `shouldBe` ("%dabc%d", [3, 4])
            readErrorFormat "" "%dabc%1$d" [] `shouldBe` ("%dabc%d", [0, 1])

        it "Numeric value is not removed when succeeding $ is not found even if its format is invalid" $ do
            readErrorFormat "" "%3dabc%1$d" [] `shouldBe` ("%3dabc%d", [0, 1])

    describe "Format error message" $ do
        let path = [KeyPointer "path"]
        let pf v = (M.!?) (M.fromList [("path", v)]) . \p -> showPath p True

        it "Error message is given explicitly" $ do
            formatError M.empty (const Nothing) (ValidationError path $ ErrorString "error message")
                `shouldBe` "path: error message"

        it "Change path in error message" $ do
            formatError M.empty (pf "abc") (ValidationError path $ ErrorString "error message")
                `shouldBe` "abc: error message"

        it "Default message when value is missing" $ do
            formatError M.empty (const Nothing) (ValidationError path $ ValueMissing)
                `shouldBe` "path: Value is not found"

        it "Change path in value missing error" $ do
            formatError M.empty (pf "abc") (ValidationError path $ ValueMissing)
                `shouldBe` "abc: Value is not found"

        it "Format of 'missing' key is used when value is missing" $ do
            formatError (M.fromList [("missing", "Value of %s is missing")]) (const Nothing) (ValidationError path $ ValueMissing)
                `shouldBe` "Value of path is missing"

        it "Default message when value is mismatch" $ do
            formatError M.empty (const Nothing) (ValidationError path $ TypeMismatch (Proxy :: Proxy Int))
                `shouldBe` "path: Value is not convertible"

        it "Change path in mismatch error" $ do
            formatError M.empty (pf "abc") (ValidationError path $ TypeMismatch (Proxy :: Proxy Int))
                `shouldBe` "abc: Value is not convertible"

        it "Format of 'mismatch' key is used when value is mismatch" $ do
            formatError (M.fromList [("mismatch", "Value of %s has invalid type")]) (const Nothing) (ValidationError path $ TypeMismatch (Proxy :: Proxy Int))
                `shouldBe` "Value of path has invalid type"

        let failure h = VerificationFailure (Proxy :: Proxy TestVerifier) h

        it "Default message of verification error" $ do
            formatError M.empty (const Nothing) (ValidationError path $ failure 0)
                `shouldBe` "path: Value violates test"

        it "Change path in verification error" $ do
            formatError M.empty (pf "abc") (ValidationError path $ failure 0)
                `shouldBe` "abc: Value violates test"

        it "Verifier identifier is the key of the format for the error it causes" $ do
            formatError (M.fromList [("test", "Value of %s is invalid (%d, %s)")]) (const Nothing) (ValidationError path $ failure 0)
                `shouldBe` "Value of path is invalid (1, xyz)"

        it "Not all verifier arguments are needed to be used" $ do
            formatError (M.fromList [("test", "Value of %s is invalid (%d)")]) (const Nothing) (ValidationError path $ failure 0)
                `shouldBe` "Value of path is invalid (1)"

        it "Orders of verifier arguments can be specified in message format" $ do
            formatError (M.fromList [("test", "(%1$d) Value of %0$s is invalid (%2$s)")]) (const Nothing) (ValidationError path $ failure 0)
                `shouldBe` "(1) Value of path is invalid (xyz)"