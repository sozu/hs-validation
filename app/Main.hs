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
    let res = fromJSON $ object [ ("int1", Number 4)
                                , ("int2", Number 5)
                                , ("str1", J.String "abedef")
                                , ("str2", J.String "abede")
                                ] :: Result Values'

    case res of
        Success obj -> do
            forM_ (errorsOf obj) $ \e -> do
                print $ formatError errorFormats (const Nothing) e
            print $ value $ int1' obj
            print $ value $ int2' obj
            print $ value $ str1' obj
            print $ value $ str2' obj
        Error e -> print e