{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Validation.Verifiers.Utilities where

import GHC.Exts
import GHC.TypeLits
import Data.Proxy
import Text.Printf

class BoolLitValue (b :: Bool) where
    boolLitValue :: proxy b -> Bool

instance BoolLitValue 'True where
    boolLitValue _ = True
instance BoolLitValue 'False where
    boolLitValue _ = False

-- | A type whose constructors are used as promoted types in verifier declarations.
data RealLit = IntLit Nat
             | NumericLit Symbol

-- | Declares a method to get a real number from the instance type.
class RealValue a (r :: RealLit) where
    realValue :: Proxy r -> a

instance (KnownNat n, Num a) => RealValue a ('IntLit n) where
    realValue _ = fromInteger $ natVal (Proxy :: Proxy n)
instance (KnownSymbol n, Num a, Read a) => RealValue a ('NumericLit n) where
    realValue _ = read $ symbolVal (Proxy :: Proxy n)
