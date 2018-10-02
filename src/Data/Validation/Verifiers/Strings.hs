{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Validation.Verifiers.Strings where

import GHC.TypeLits
import Data.Proxy
import Data.Char
import Language.Haskell.TH hiding (Range)
import Data.Validation.Validation
import Data.Validation.Verifiers.Utilities

-- ----------------------------------------------------------------
-- String verifiers
-- ----------------------------------------------------------------

-- | Verifier applied to @String@ which verifies its length falls between @min@ and @max@.
data Length s (min :: Nat) (max :: Nat)

instance (KnownNat min, KnownNat max) => Verifier (Length String min max) where
    type VerifiableType (Length String min max) = String
    type VerifierSpec (Length String min max) = '[Integer, Integer]

    verifierSpec _ = ("str.length", natVal (Proxy :: Proxy min) `ACons` natVal (Proxy :: Proxy max) `ACons` ANil)

    verify p v = if len < min || len > max then Left () else Right v
        where
            len = toInteger $ length v
            (min, max) = (,) <-$ verifierArgs p

    verificationFailure p path _ = "Length of " ++ showPath path True
                                    ++ " must be larger than " ++ show min
                                    ++ " and smaller than " ++ show max
        where
            (min, max) = (,) <-$ verifierArgs p

-- | Verifier applied to @String@ which verifies its length is longer or shorter than @len@.
data CompLength (los :: Bool) s (len :: Nat)

-- | Type synonym for "longer than" verifier.
type Longer s (len :: Nat) = CompLength 'True s len
-- | Type synonym for "shorter than" verifier.
type Shorter s (len :: Nat) = CompLength 'False s len

instance (KnownNat len, BoolLitValue los) => Verifier (CompLength los String len) where
    type VerifiableType (CompLength los String len) = String
    type VerifierSpec (CompLength los String len) = '[Integer]

    verifierSpec _ = (ident, natVal (Proxy :: Proxy len) `ACons` ANil)
        where
            ident = if boolLitValue (Proxy :: Proxy los) then "str.longer" else "str.shorter"

    verify p v = if (los' && len > threshold) || (not los' && len < threshold) then Right v else Left ()
        where
            len = toInteger $ length v
            threshold = id <-$ verifierArgs p
            los' = boolLitValue (Proxy :: Proxy los)

    verificationFailure p path _ = "Length of " ++ showPath path True
                                    ++ " must be " ++ if los' then "longer" else "shorter" ++ " than " ++ show threshold
        where
            threshold = id <-$ verifierArgs p
            los' = boolLitValue (Proxy :: Proxy los)


-- | Instances of this class are designed to be used with @CharOf@ verifier.
--
-- With @CharOf@ verifier, input string is verified whether it consists of specified characters only.
class CharFilter f where
    -- | This symbol is used as the first argument of @CharOf@ verifier.
    type CharFilterSpec f :: Symbol
    -- | Filter function of characters in input string.
    filterChar :: Char -- ^ A character.
               -> Bool -- ^ True when this filter pass the character.

-- | Verifier applied to @String@ and verify it consists of characters all of which are passed filter function defined on @f@.
data CharOf f

instance (CharFilter f, KnownSymbol (CharFilterSpec f)) => Verifier (CharOf f) where
    type VerifiableType (CharOf f) = String
    type VerifierSpec (CharOf f) = '[String]

    verifierSpec _ = ("str.charOf." ++ spec, spec `ACons` ANil)
        where
            spec = symbolVal (Proxy :: Proxy (CharFilterSpec f))

    verify p [] = Right []
    verify p (c:cs)
        | filterChar @f c = (c:) <$> verify p cs
        | otherwise = Left ()

    verificationFailure p path _ = "Every character must fulfill '" ++ spec ++ "'"
        where
            spec = id <-$ verifierArgs p

-- | TH function declaring @CharacterFilter@ data type.
declareCharFilter :: String -- ^ Name of generating data type.
                  -> String -- ^ Specifier of the type used for the @CharFilterSpec@ symbol.
                  -> Name -- ^ The name of filter function.
                  -> Q [Dec] -- ^ Declarations of data type and @CharFilter@ instance for it.
declareCharFilter n s f = (:) <$> (dataD (cxt []) cn [] Nothing [] []) <*> [d|
        instance CharFilter $(conT cn) where
            type CharFilterSpec $(conT cn) = $(litT $ strTyLit s)
            filterChar = $(varE f)
    |]
    where
        cn = mkName n