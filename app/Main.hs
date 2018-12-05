{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad
import Data.Proxy
import qualified Data.Map as M
import Data.Aeson as J
import Data.Validation

type LessThan4_5 = $(lt 4.5) Integer
type LongerThan5 = Longer String 5

data Values = Values { int1 :: Integer :? '[LessThan4_5]
                     , int2 :: Integer :? '[LessThan4_5]
                     , str1 :: String :? '[LongerThan5]
                     , str2 :: String :? '[LongerThan5]
                     }

$(validatable [''Values])

errorFormats :: M.Map String String
errorFormats = M.fromList [
    ("real.lt", "%s: given value is not less than %f")
  , ("str.longer", "%v: given string is not longer than %d")
  ]

main :: IO ()
main = do
    let (Success v') = fromJSON $ object [ ("int1", Number 4)
                                         , ("int2", Number 3)
                                         , ("str1", J.String "abedef")
                                         , ("str2", J.String "abedefg")
                                         ] :: Result Values'

    case validate v' of
        Just v -> do
            let result = (int1 .! v, int2 .! v, str1 .! v, str2 .! v) :: (Integer, Integer, String, String)
            print result
        Nothing -> do
            forM_ (errorsOf v') $ \e -> do
                print $ formatError errorFormats (const Nothing) e