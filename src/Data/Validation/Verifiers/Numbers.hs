{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Validation.Verifiers.Numbers where

import GHC.TypeLits
import Data.Proxy
import Data.Char
import Language.Haskell.TH hiding (Range)
import Data.Validation.Validation
import Data.Validation.Verifiers.Utilities

-- ----------------------------------------------------------------
-- Real number verifiers
-- ----------------------------------------------------------------

-- | Verifier applied to @t@ comparing a value with the real value obtained from @a@.
--
-- @gol@ specifies whether the value should be greater or smaller than @a@.
-- The value can be equal to @a@ if and only if @incl@ is @'True@.
--
-- Note that the values are first converted into @Double@ and then compared in @Double@ precision.
-- Therefore, the numerical equality is not guaranteed when the value is the result of calculation.
-- (ex. @1.2 * 3 == 3.6@ falls into @False@)
data CompReal (gol :: Bool) (incl :: Bool) (a :: RealLit) t

instance (Real t, Ord t, RealValue Double a, BoolLitValue gol, BoolLitValue incl) => Verifier (CompReal gol incl a t) where
    type VerifiableType (CompReal gol incl a t) = t
    type VerifierSpec (CompReal gol incl a t) = '[Double]

    verifierSpec _ = (ident, realValue @Double (Proxy :: Proxy a) `ACons` ANil)
        where
            gol' = boolLitValue (Proxy :: Proxy gol)
            incl' = boolLitValue (Proxy :: Proxy incl)
            ident
                | gol' && incl'       = "real.gte"
                | (not gol') && incl' = "real.lte"
                | gol' && (not incl') = "real.gt"
                | otherwise           = "real.lt"

    verify p v = if comp then Right v else Left ()
        where
            v' = realToFrac v
            threshold = id <-$ verifierArgs p
            gol' = boolLitValue (Proxy :: Proxy gol)
            incl' = boolLitValue (Proxy :: Proxy incl)
            comp
                | gol' && incl'       = v' >= threshold
                | (not gol') && incl' = v' <= threshold
                | gol' && (not incl') = v' > threshold
                | otherwise           = v' < threshold

    verificationFailure p path _ = "Value of " ++ showPath path True
                                    ++ " must be " ++ repGol ++ " than " ++ show threshold
        where
            threshold = id <-$ verifierArgs p
            repGol = if boolLitValue (Proxy :: Proxy gol) then "larger" else "smaller"

-- | Verifier appplied to @t@ checking a value is inside (or outside) the range from @a@ to @b@.
--
-- Although the same verification can be realized by the combination of @CompReal@ verifiers,
-- its output (error messages and so on) is not desirable in many cases.
-- This verifier may be a good option when the application focuses on the value range rather than independent minimum and maximum.
data RangeReal (io :: Bool) (aincl :: Bool) (bincl :: Bool) (a :: RealLit) (b :: RealLit) t

instance ( Real t, Ord t
         , RealValue Double a, RealValue Double b
         , BoolLitValue io, BoolLitValue aincl, BoolLitValue bincl
         ) => Verifier (RangeReal io aincl bincl a b t) where
    type VerifiableType (RangeReal io aincl bincl a b t) = t
    type VerifierSpec (RangeReal io aincl bincl a b t) = '[Double, Double]

    verifierSpec _ = (ident, realValue @Double (Proxy :: Proxy a) `ACons` realValue @Double (Proxy :: Proxy b) `ACons` ANil)
        where
            io' = boolLitValue (Proxy :: Proxy io)
            identA = if boolLitValue (Proxy :: Proxy aincl)
                            then if io' then ".gte" else ".lte"
                            else if io' then ".gt" else ".lt"
            identB = if boolLitValue (Proxy :: Proxy bincl)
                            then if io' then ".lte" else ".gte"
                            else if io' then ".lt" else ".gt"
            ident = if io' then "real" ++ identA ++ identB
                           else "real" ++ identA ++ identB

    verify p v = if (io' && checkA && checkB) || (not io' && (checkA || checkB)) then Right v else Left ()
        where
            v' = realToFrac v
            (min, max) = (,) <-$ verifierArgs p
            io' = boolLitValue (Proxy :: Proxy io)
            checkA = if boolLitValue (Proxy :: Proxy aincl)
                            then (io' && v' >= min) || (not io' && v' <= min)
                            else (io' && v' > min) || (not io' && v' < min)
            checkB = if boolLitValue (Proxy :: Proxy bincl)
                            then (io' && v' <= max) || (not io' && v' >= max)
                            else (io' && v' < max) || (not io' && v' > max)

    verificationFailure p path _ = "Value of " ++ showPath path True
                                    ++ " must be larger than " ++ show min
                                    ++ " and smaller than " ++ show max
        where
            (min, max) = (,) <-$ verifierArgs p

makeCompReal :: (Real t) => Bool -> Bool -> t -> TypeQ
makeCompReal gol incl v = [t|
        CompReal $(liftBool gol) $(liftBool incl) $(realLit $ realToFrac v)
    |]

liftBool :: Bool -> TypeQ
liftBool True = [t| 'True |]
liftBool False = [t| 'False |]

-- | Generates type expression of @RealLit@ having a @Double@ value.
realLit :: Double -- ^ Value of generated type.
        -> TypeQ -- ^ Type expression.
realLit v = [t| NumericLit $(litT $ strTyLit $ show v) |]

-- | Shortcut to generate "less than" verifier expression.
lt :: (Real t)
   => t -- ^ Value of the verifier.
   -> TypeQ -- ^ Type expression of the verifier.
lt v = makeCompReal False False v

-- | Shortcut to generate "less than or equal to" verifier expression.
lte :: (Real t)
    => t -- ^ Value of the verifier.
    -> TypeQ -- ^ Type expression of the verifier.
lte v = makeCompReal False True v

-- | Shortcut to generate "greater than" verifier expression.
gt :: (Real t)
   => t -- ^ Value of the verifier.
   -> TypeQ -- ^ Type expression of the verifier.
gt v = makeCompReal True False v

-- | Shortcut to generate "greater than or equal to" verifier expression.
gte :: (Real t)
    => t -- ^ Value of the verifier.
    -> TypeQ -- ^ Type expression of the verifier.
gte v = makeCompReal True True v

makeRangeReal :: (Real t) => Bool -> Bool -> Bool -> t -> t -> TypeQ
makeRangeReal io aincl bincl a b = [t|
        RangeReal $(liftBool io) $(liftBool aincl) $(liftBool bincl) $(realLit $ realToFrac a) $(realLit $ realToFrac b)
    |]

inside :: (Real t, Show t)
       => t
       -> t
       -> Bool
       -> Bool
       -> TypeQ
inside a b aincl bincl
    | a <= b    = makeRangeReal True aincl bincl a b
    | otherwise = fail $ "First value (" ++ show a ++ ") must be smaller or equal to second value (" ++ show b ++ ") on inside verifier"

outside :: (Real t, Show t)
        => t
        -> t
        -> Bool
        -> Bool
        -> TypeQ
outside a b aincl bincl
    | a <= b = makeRangeReal False aincl bincl a b
    | otherwise = fail $ "First value (" ++ show a ++ ") must be smaller or equal to second value (" ++ show b ++ ") on outside verifier"

-- ----------------------------------------------------------------
-- Integral verifiers
-- ----------------------------------------------------------------

-- | Verifier applied to @t@ which verifies a value falls into inside or outside (@io == 'True@) of the range [@min@, @max@].
--
-- The range means the closed interval which includes both endpoints.
data Range (io :: Bool) t (min :: Nat) (max :: Nat)

-- | Type synonym representing "in range" verifier.
type InRange t (min :: Nat) (max :: Nat) = Range 'True t min max
-- | Type synonym representing "out of range" verifier.
type OutOfRange t (min :: Nat) (max :: Nat) = Range 'False t min max

instance (Integral t, BoolLitValue io, KnownNat min, KnownNat max, min <= max) => Verifier (Range io t min max) where
    type VerifiableType (Range io t min max) = t
    type VerifierSpec (Range io t min max) = '[Integer, Integer]

    verifierSpec _ = (ident, natVal (Proxy :: Proxy min) `ACons` natVal (Proxy :: Proxy max) `ACons` ANil)
        where
            ident = if boolLitValue (Proxy :: Proxy io) then "int.inRange" else "int.outOfRange"

    verify p v = let isOutside = iv < min || iv > max
                 in if (io' && not isOutside) || (not io' && isOutside) then Right v else Left ()
        where
            iv = toInteger v
            io' = boolLitValue (Proxy :: Proxy io)
            (min, max) = (,) <-$ verifierArgs p

    verificationFailure p path _ = "Value of " ++ showPath path True
                                    ++ " must be larger than " ++ show min
                                    ++ " and smaller than " ++ show max
        where
            (min, max) = (,) <-$ verifierArgs p

-- | Verifier applied to @t@ which compares a value with given integral @a@.
--
-- @gol@ specifies whether the value should be greater or smaller than @a@.
-- The value can be equal to @a@ if and only if @incl@ is @'True@.
data Comp (gol :: Bool) (incl :: Bool) t (a :: Nat)

-- | Type synonym for "less than" verifier.
type LT t (a :: Nat) = Comp 'False 'False t a
-- | Type synonym for "greather than" verifier.
type GT t (a :: Nat) = Comp 'True 'False t a
-- | Type synonym for "less than or equal to" verifier.
type LTE t (a :: Nat) = Comp 'False 'True t a
-- | Type synonym for "greater than or equal to" verifier.
type GTE t (a :: Nat) = Comp 'True 'True t a

instance (Integral t, BoolLitValue gol, BoolLitValue incl, KnownNat a) => Verifier (Comp gol incl t a) where
    type VerifiableType (Comp gol incl t a) = t
    type VerifierSpec (Comp gol incl t a) = '[Integer]

    verifierSpec _ = (ident, natVal (Proxy :: Proxy a) `ACons` ANil)
        where
            gol' = boolLitValue (Proxy :: Proxy gol)
            incl' = boolLitValue (Proxy :: Proxy incl)
            ident
                | gol' && incl'     = "int.gte"
                | not gol' && incl' = "int.lte"
                | gol' && not incl' = "int.gt"
                | otherwise         = "int.lt"

    verify p v = if comp then Right v else Left ()
        where
            iv = toInteger v
            gol' = boolLitValue (Proxy :: Proxy gol)
            incl' = boolLitValue (Proxy :: Proxy incl)
            threshold = id <-$ verifierArgs p
            comp
                | gol' && incl'     = iv >= threshold
                | not gol' && incl' = iv <= threshold
                | gol' && not incl' = iv > threshold
                | otherwise         = iv < threshold

    verificationFailure p path _ = "Value of " ++ showPath path True
                                    ++ " must be " ++ repGol ++ " than " ++ show threshold
        where
            threshold = id <-$ verifierArgs p
            repGol = if boolLitValue (Proxy :: Proxy gol) then "larger" else "smaller"