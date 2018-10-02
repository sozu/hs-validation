{-# LANGUAGE OverloadedStrings #-}

module Data.Validation.ValidationSpec where

import Test.Hspec
import Data.Proxy
import Data.Aeson.BetterErrors
import Data.Validation

data V1 = V1 { f11 :: F String
             , f12 :: F Int
             , f13 :: F Bool
             }

instance FromJSONBetterErrors V1 where
    fromJSONBetterErrors = V1 <$> asField (Proxy :: Proxy (F String)) (KeyPointer "f11")
                              <*> asField (Proxy :: Proxy (F Int)) (KeyPointer "f12")
                              <*> asField (Proxy :: Proxy (F Bool)) (KeyPointer "f13")

data V2 = V2 { f21 :: F (Maybe String)
             , f22 :: F (Maybe Int)
             }

instance FromJSONBetterErrors V2 where
    fromJSONBetterErrors = V2 <$> asField (Proxy :: Proxy (F (Maybe String))) (KeyPointer "f21")
                              <*> asField (Proxy :: Proxy (F (Maybe Int))) (KeyPointer "f22")

data V3 = V3 { f31 :: F [F String]
             , f32 :: F [F Int]
             }

instance FromJSONBetterErrors V3 where
    fromJSONBetterErrors = V3 <$> asField (Proxy :: Proxy (F [F String])) (KeyPointer "f31")
                              <*> asField (Proxy :: Proxy (F [F Int])) (KeyPointer "f32")

spec :: Spec
spec = do
    describe "validation of primitive types" $ do
        it "valid" $ do
            let res = parse (fromJSONBetterErrors :: Parse ValidationError' V1) $ "{\
                        \ \"f11\": \"test\", \
                        \ \"f12\": 12, \
                        \ \"f13\": true \
                        \ }"
            case res of
                Right v -> do
                    value (f11 v) `shouldBe` Just "test"
                    value (f12 v) `shouldBe` Just 12
                    value (f13 v) `shouldBe` Just True
                Left e -> expectationFailure (show e)

        it "invalid integer" $ do
            let res = parse (fromJSONBetterErrors :: Parse ValidationError' V1) $ "{\
                        \ \"f11\": \"test\", \
                        \ \"f12\": \"abc\", \
                        \ \"f13\": true \
                        \ }"
            case res of
                Right v -> do
                    value (f11 v) `shouldBe` Just "test"
                    value (f12 v) `shouldBe` Nothing
                    cause (f12 v) `shouldBe` Just (TypeMismatch (Proxy :: Proxy Int))
                    value (f13 v) `shouldBe` Just True
                Left e -> expectationFailure (show e)

    describe "validation of Maybes" $ do
        it "valid" $ do
            let res = parse (fromJSONBetterErrors :: Parse ValidationError' V2) $ "{\
                        \ \"f21\": \"test\" \
                        \ }"
            case res of
                Right v -> do
                    value (f21 v) `shouldBe` Just (Just "test")
                    value (f22 v) `shouldBe` Just Nothing
                Left e -> expectationFailure (show e)

        it "invalid interger" $ do
            let res = parse (fromJSONBetterErrors :: Parse ValidationError' V2) $ "{\
                        \ \"f21\": \"test\", \
                        \ \"f22\": \"abc\" \
                        \ }"
            case res of
                Right v -> do
                    value (f21 v) `shouldBe` Just (Just "test")
                    value (f22 v) `shouldBe` Nothing
                    cause (f22 v) `shouldBe` Just (TypeMismatch (Proxy :: Proxy (Maybe Int)))
                Left e -> expectationFailure (show e)

    describe "validation of lists" $ do
        it "valid" $ do
            let res = parse (fromJSONBetterErrors :: Parse ValidationError' V3) $ "{\
                        \ \"f31\": [\"abc\", \"def\"], \
                        \ \"f32\": [12, 34] \
                        \ }"
            case res of
                Right v -> do
                    (>>= return . map value) (value $ f31 v) `shouldBe` Just [Just "abc", Just "def"]
                    (>>= return . map value) (value $ f32 v) `shouldBe` Just [Just 12, Just 34]
                Left e -> expectationFailure (show e)

        it "invalid integer" $ do
            let res = parse (fromJSONBetterErrors :: Parse ValidationError' V3) $ "{\
                        \ \"f31\": [\"abc\", \"def\"], \
                        \ \"f32\": [12, \"invalid\"] \
                        \ }"
            case res of
                Right v -> do
                    (>>= return . map value) (value $ f31 v) `shouldBe` Just [Just "abc", Just "def"]
                    (>>= return . map value) (value $ f32 v) `shouldBe` Just [Just 12, Nothing]
                    (>>= return . map cause) (value $ f32 v) `shouldBe` Just [Nothing, Just (TypeMismatch (Proxy :: Proxy Int))]
                Left e -> expectationFailure (show e)
