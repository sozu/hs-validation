{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Data.Validation.Verifiers.VerifiersSpec where

import Test.Hspec
import Control.Monad
import Data.Proxy
import Data.Maybe (fromJust)
import Data.Aeson
import Data.Char
import Data.Validation

data TestVerifier

instance Verifier TestVerifier where
    type VerifiableType TestVerifier = String
    type VerifierSpec TestVerifier = [String, String, String]
    verifierSpec _ = ("test", "abc" `ACons` "def" `ACons` "ghi" `ACons` ANil)
    verify p v = let (a,b,c) = (,,) <-$ verifierArgs p
                 in if v `elem` [a,b,c] then Right v else Left ()

data TestData = TestData { f1 :: String :? '[TestVerifier]
                         , f2 :: [String :? '[TestVerifier]]
                         , f3 :: Maybe (String :? '[TestVerifier])
                         } deriving (Show, Eq)

$(validatable [''TestData])

spec :: Spec
spec = do
    let vf = VerificationFailure (Proxy :: Proxy TestVerifier) ()

    describe "Check verification method returns expected resulT" $ do
        it "When input is valid data" $ do
            let r = fromJSON $ object [ ("f1", "abc")
                                      , ("f2", toJSON (["def", "ghi"] :: [String]))
                                      , ("f3", "ghi")
                                      ] :: Result TestData'
            case r of
                Success r' -> do
                    let v = validate r'
                    safeData . f1 <$> v `shouldBe` Just "abc"
                    map safeData . f2 <$> v `shouldBe` Just ["def", "ghi"]
                    (>>= return . safeData) . f3 <$> v `shouldBe` Just (Just "ghi")
                Error e -> expectationFailure e

        it "When input is valid data and no data for Maybe field" $ do
            let r = fromJSON $ object [ ("f1", "abc")
                                      , ("f2", toJSON (["def", "ghi"] :: [String]))
                                      ] :: Result TestData'
            case r of
                Success r' -> do
                    let v = validate r'
                    safeData . f1 <$> v `shouldBe` Just "abc"
                    map safeData . f2 <$> v `shouldBe` Just ["def", "ghi"]
                    f3 <$> v `shouldBe` Just Nothing
                Error e -> expectationFailure e

        it "When input has invalid data for string scalar field" $ do
            let r = fromJSON $ object [ ("f1", "xxx")
                                      , ("f2", toJSON (["def", "ghi"] :: [String]))
                                      ] :: Result TestData'
            case r of
                Success r' -> do
                    validate r' `shouldBe` Nothing
                    value (f1' r') `shouldBe` Nothing
                Error e -> expectationFailure e

        it "When input has invalid data for string list field" $ do
            let r = fromJSON $ object [ ("f1", "abc")
                                      , ("f2", toJSON (["def", "xxx"] :: [String]))
                                      ] :: Result TestData'
            case r of
                Success r' -> do
                    validate r' `shouldBe` Nothing
                    value (f2' r') `shouldBe` Nothing
                Error e -> expectationFailure e

        it "When input has invalid data for Maybe string field" $ do
            let r = fromJSON $ object [ ("f1", "abc")
                                      , ("f2", toJSON (["def", "ghi"] :: [String]))
                                      , ("f3", "xxx")
                                      ] :: Result TestData'
            case r of
                Success r' -> do
                    validate r' `shouldBe` Nothing
                    value (f3' r') `shouldBe` Nothing
                Error e -> expectationFailure e