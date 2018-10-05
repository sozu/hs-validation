{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Validation.THJSONSpec where

import Test.Hspec
import GHC.Generics
import qualified Data.Map as M
import Data.Proxy
import Data.Aeson
import Data.Validation

data ForInt = ForInt { int :: Int } deriving (Eq, Show, Generic)
data ForStr = ForStr { str :: String } deriving (Eq, Show, Generic)
data ForIntList = ForIntList { intlist :: [Int] } deriving (Eq, Show, Generic)
data ForStrList = ForStrList { strlist :: [String] } deriving (Eq, Show, Generic)
data ForIntMap = ForIntMap { intmap :: M.Map String Int } deriving (Eq, Show, Generic)
data ForVal = ForVal { val :: ForInt } deriving (Eq, Show, Generic)
data ForValList = ForValList { vallist :: [ForInt] } deriving (Eq, Show, Generic)
data ForValMap = ForValMap { valmap :: M.Map String ForInt } deriving (Eq, Show, Generic)
data ForIntMaybe = ForIntMaybe { intmaybe :: Maybe Int } deriving (Eq, Show, Generic)
data ForValMaybe = ForValMaybe { valmaybe :: Maybe ForInt } deriving (Eq, Show, Generic)

$(validatable [''ForInt, ''ForStr, ''ForIntList, ''ForStrList, ''ForIntMap, ''ForVal, ''ForValList, ''ForValMap, ''ForIntMaybe, ''ForValMaybe])

spec :: Spec
spec = do
    describe "validation of int field" $ do
        it "valid" $ do
            let r = fromJSON $ object [("int", Number 1)] :: Result ForInt'
            case r of
                Success r' -> validate r' `shouldBe` Just (ForInt 1)
                Error e -> expectationFailure e

    describe "validation of string field" $ do
        it "valid" $ do
            let r = fromJSON $ object [("str", String "abc")] :: Result ForStr'
            case r of
                Success r' -> validate r' `shouldBe` Just (ForStr "abc")
                Error e -> expectationFailure e

    describe "validation of int list field" $ do
        it "valid" $ do
            let r = fromJSON $ object [("intlist", toJSON ([1,2,3] :: [Int]))] :: Result ForIntList'
            case r of
                Success r' -> validate r' `shouldBe` Just (ForIntList [1,2,3])
                Error e -> expectationFailure e

    describe "validation of int map field" $ do
        it "valid" $ do
            let r = fromJSON $ object [("intmap", object [("abc", Number 1), ("def", Number 2)])] :: Result ForIntMap'
            case r of
                Success r' -> validate r' `shouldBe` Just (ForIntMap $ M.fromList [("abc", 1), ("def", 2)])
                Error e -> expectationFailure e

    describe "validation of string list field" $ do
        it "valid" $ do
            let r = fromJSON $ object [("strlist", toJSON (["abc", "def", "ghi"] :: [String]))] :: Result ForStrList'
            case r of
                Success r' -> validate r' `shouldBe` Just (ForStrList ["abc", "def", "ghi"])
                Error e -> expectationFailure e

    describe "validation of validatable field" $ do
        it "valid" $ do
            let r = fromJSON $ object [("val", object [("int", Number 1)])] :: Result ForVal'
            case r of
                Success r' -> validate r' `shouldBe` Just (ForVal (ForInt 1))
                Error e -> expectationFailure e

    describe "validation of validatable list field" $ do
        it "valid" $ do
            let r = fromJSON $ object [("vallist", toJSON [ object [("int", Number 1)]
                                                          , object [("int", Number 2)]
                                                          , object [("int", Number 3)]
                                                          ])] :: Result ForValList'
            case r of
                Success r' -> validate r' `shouldBe` Just (ForValList [ForInt 1, ForInt 2, ForInt 3])
                Error e -> expectationFailure e

    describe "validation of validatable map field" $ do
        it "valid" $ do
            let r = fromJSON $ object [("valmap", object [ ("abc", object [("int", Number 1)])
                                                         , ("def", object [("int", Number 2)])
                                                         ])] :: Result ForValMap'
            case r of
                Success r' -> validate r' `shouldBe` Just (ForValMap (M.fromList [("abc", ForInt 1), ("def", ForInt 2)]))
                Error e -> expectationFailure e

    describe "validation of int maybe field" $ do
        it "valid" $ do
            let r = fromJSON $ object [("intmaybe", Number 1)] :: Result ForIntMaybe'
            case r of
                Success r' -> validate r' `shouldBe` Just (ForIntMaybe $ Just 1)
                Error e -> expectationFailure e

        it "null" $ do
            let r = fromJSON $ object [("intmaybe", Null)] :: Result ForIntMaybe'
            case r of
                Success r' -> validate r' `shouldBe` Just (ForIntMaybe $ Nothing)
                Error e -> expectationFailure e

        it "empty" $ do
            let r = fromJSON $ object [] :: Result ForIntMaybe'
            case r of
                Success r' -> validate r' `shouldBe` Just (ForIntMaybe $ Nothing)
                Error e -> expectationFailure e