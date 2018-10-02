{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Data.Validation.Verifiers.NumbersSpec where

import Test.Hspec
import Data.Proxy
import Data.Validation

spec :: Spec
spec = do
    describe "CompReal" $ do
        it "valid gte" $ do
            verify (Proxy :: Proxy $(gte @Double 10)) 5 `shouldBe` Left 
            --errorMessage (Proxy :: Proxy ($(gte @Double 10) Double)) "Not > %f" `shouldBe` "Not > 10"