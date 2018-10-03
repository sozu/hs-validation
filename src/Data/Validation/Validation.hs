{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Data.Validation.Validation where

import GHC.Exts
import GHC.Generics
import Control.Applicative
import Control.Monad
import Control.Monad.Except (catchError, throwError)
import Data.Proxy
import Data.Dynamic
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T
import Data.Maybe (maybe)
import Text.Printf
import Data.Aeson as J
import qualified Data.Aeson.BetterErrors as JB
import Data.Aeson.Types
import Debug.Trace

-- ----------------------------------------------------------------
-- Types and functions used in validation
-- ----------------------------------------------------------------

-- | This type indicates the location of parsing value in data source.
data Pointer = RawPointer
             | IndexPointer Int
             | KeyPointer String
             deriving (Show, Eq)

type PointerPath = [Pointer]

showPath :: PointerPath
         -> Bool
         -> String
showPath [] True = "(root)"
showPath [] False = ""
showPath (RawPointer : ps) True = showPath ps True
showPath (RawPointer : ps) False = showPath ps False
showPath (IndexPointer i : ps) _ = "[" ++ show i ++ "]" ++ showPath ps False
showPath (KeyPointer k : ps) True = k ++ showPath ps False
showPath (KeyPointer k : ps) False = "." ++ k ++ showPath ps False

-- | Data source from which each field value is assigned.
data Source = StringValidatable String
            | ByteStringValidatable B.ByteString
            | TextValidatable T.Text
            | EmptyValidatable
            deriving (Show, Eq, Generic)

-- | Declares a method to convert a value to Source.
class ToSource a where
    -- | Converts a value to Source.
    toSource :: a -- ^ A value.
             -> Source -- ^ Converted Source value.

-- | Declares conversions from JSON component to Source.
instance ToSource Value where
    toSource (Object v) = ByteStringValidatable (encode v)
    toSource (Array v) = ByteStringValidatable (encode v)
    toSource (String v) = TextValidatable (v)
    toSource (Number v) = StringValidatable (show v)
    toSource (Bool v) = StringValidatable (if v then "true" else "false")
    toSource Null = StringValidatable "null"

instance ToSource T.Text where
    toSource = TextValidatable

-- | Declares types of causes for validation errors.
data ValidationError' = ErrorString String
                      | ValueMissing
                      | forall t. TypeMismatch (Proxy t)
                      | forall v. (Verifier v, AllFormattableArg (VerifierSpec v)) => VerificationFailure (Proxy v) (FailureHint v)

instance Show ValidationError' where
    show (ErrorString s) = s
    show (ValueMissing) = "Value is not found"
    show (TypeMismatch _) = "Value is not convertible"
    show (VerificationFailure v hint) = "Invalid value"

instance Eq ValidationError' where
    ErrorString s1 == ErrorString s2 = s1 == s2
    ValueMissing == ValueMissing = True
    TypeMismatch _ == TypeMismatch _ = True
    VerificationFailure v1 _ == VerificationFailure v2 _ = eqVerifier v1 v2
    _ == _ = False

-- | The information of a validation error holding the location in the data source and the cause of the error.
data ValidationError = ValidationError PointerPath ValidationError' deriving (Eq)

(!@) :: PointerPath
     -> ValidationError'
     -> ValidationError
(!@) path e = ValidationError path e

instance Show ValidationError where
    show (ValidationError path err) = "[" ++ showPath path True ++ "] "
            ++ case err of
                ErrorString s -> s
                ValueMissing -> "Value is not found"
                TypeMismatch _ -> "Value is not convertible"
                VerificationFailure v hint -> verificationFailure v path hint

-- | Format error message with given message format.
--
-- Each format string should conform to the format defined by @Text.Printf@.
-- By default, a message is rendered with arguments where
-- the first one is the string representing the location of the error
-- and values of @verifierArgs@ follow in declared order.
-- When the number of variables in the format is less than the arguments, redundant arguments are omitted.
--
-- Also, extended variable format which controls the order of arguments is available. 
-- It contains the index of arguments which the variable refers to in form of @"%index$format"@.
-- @index@ should be a natural number and @0@ means the first argument.
-- When a variable does not have @index@ specifier, its index is determined by adding 1 to the index of previous variable
-- or set to 0 if it is the first variable.
formatError :: M.Map String String -- ^ Mapping from verifier name to error message format.
            -> (PointerPath -> Maybe String) -- ^ Function converting a path of the location to its representaion.
            -> ValidationError -- ^ Validation error to format.
            -> String -- ^ Formatted message.
formatError msgs pf (ValidationError path e)
    | ErrorString s <- e              = printf (defaultFormat s) name
    | ValueMissing <- e               = printf (format "missing" "Value is not found") name
    | TypeMismatch _ <- e             = printf (format "mismatch" "Value is not convertible") name
    | VerificationFailure v hint <- e =
        let (spec, args) = verifierSpec v
            fmt = msgs M.!? spec
        in case fmt of
            Nothing -> printf (defaultFormat "Value violates %s") name spec
            Just m  -> let (m', indexes) = readErrorFormat "" m []
                       in showVerificationFailure name v indexes m'
    where
        name = maybe (showPath path True) id (pf path)
        defaultFormat s = "%s: " ++ s
        format k s = maybe (defaultFormat s) id (msgs M.!? k)

readErrorFormat :: String
                -> String
                -> [Int]
                -> (String, [Int])
readErrorFormat acc [] indexes = (reverse acc, arrangeIndexes (-1) $ reverse indexes)
    where
        arrangeIndexes :: Int -> [Int] -> [Int]
        arrangeIndexes pre [] = []
        arrangeIndexes pre (i:ixs) = let ix = if i >= 0 then i else (pre+1)
                                     in ix : arrangeIndexes ix ixs
readErrorFormat acc ('%':'%':cs) indexes = readErrorFormat ('%':'%':acc) cs indexes
readErrorFormat acc ('%':cs) indexes = case readIndex [] cs of
        Just (index, consumed) -> readErrorFormat ('%':acc) (drop consumed cs) (index:indexes)
        Nothing -> readErrorFormat ('%':acc) cs (-1:indexes)
    where
        readIndex :: [Char] -> [Char] -> Maybe (Int, Int)
        readIndex [] [] = Nothing
        readIndex [] (c:cs) = if c `elem` "0123456789" then readIndex [c] cs else Nothing
        readIndex cur (c:cs)
            | c == '$'              = Just (read (reverse cur) :: Int, length cur + 1)
            | c `elem` "0123456789" = readIndex (c:cur) cs
            | otherwise             = Nothing
readErrorFormat acc (c:cs) indexes = readErrorFormat (c:acc) cs indexes

-- | Wrapper of @a@ holding the value or error cause according to the validation result of a field.
data F a = F { value :: Maybe a
             , source :: Source
             , cause :: Maybe ValidationError'
             } deriving (Generic)

-- | Instances of @Validatable@ provides a way to convert from some data source to valid object of @ValidationTarget v@.
--
-- @v@ is a data type which can hold values enough to construct @ValidationTarget v@ object,
-- and errors happened in conversion from data source to each field of @ValidationTarget v@.
--
-- To make a validatable type from existing data type defined in record syntax, use a TH function @Data.Validation.TH.validatable@.
-- @Data.Validation.TH.validatable ''A@ generates another type @A'@ which implements @Validatable A'@ with @ValidationTarget A' = A@.
-- Refer the documentation of @Data.Validation.TH.validatable@ to know the detail of the generation.
class Validatable v where
    type ValidationTarget v :: *

    -- | Returns valid object of @a@ if the validation succeeded.
    validate :: v -- ^ Object holding field values of @ValidationTarget v@ and validation errors.
             -> Maybe (ValidationTarget v) -- ^ @Just (ValidationTarget v)@ if validation succeeded, otherwise Nothing.

    -- | Returns a list of validation errors.
    errors :: PointerPath -- ^ Base path indexing the location of this value.
           -> v -- ^ Object holding field values of @ValidationTarget v@ and validation errors.
           -> [ValidationError] -- ^ List of validation errors.

-- | Returns a list of validation errors.
errorsOf :: (Validatable v)
         => v -- ^ Object holding field values of @ValidationTarget v@ and validation errors.
         -> [ValidationError] -- ^ List of validation errors.
errorsOf v = errors [] v

-- ----------------------------------------------------------------
-- Verifiers
-- ----------------------------------------------------------------

-- | Instances of this class should provide a way to verify the value of @VerifiableType v@.
--
-- @VerifierSpec v@ denotes types of type level arguments defined in the declaration of each verifier.
-- They can be obtained by @verifierArgs@ to control verification logic.
-- The instance of @Data.Validation.Verifiers.Strings.Range@ is a typical example using the arguments.
--
-- @FailureHint@ is a type having additional information of the verification failure.
-- Each instance can override this type if necessary and should implement @verify@ to return some value on failure.
-- As being contained in @VerificationFailure@ finally, the application code can use it to make more informative error message.
class (Eq (Args (VerifierSpec v))) => Verifier v where
    type VerifiableType v :: *
    type VerifierSpec v :: [*]
    type FailureHint v :: *
    type instance FailureHint v = ()

    -- | Get specification of the verifier.
    --
    -- Arguments declared as @VerifierSpec@ should be instantiated from type to value in this method.
    -- For example, type-level literals are converted into values and concatenated with @ACons@ to generate a value of @Args@.
    -- The name is an identifier of the verifier prepared for the external usage like error message configuration.
    verifierSpec :: Proxy v -- ^ Verifier type.
                 -> (String, Args (VerifierSpec v)) -- ^ Name and arguments of the verifier.

    -- | Shortcut to get arguments of the verifier.
    verifierArgs :: Proxy v -> Args (VerifierSpec v)
    verifierArgs = snd . verifierSpec

    -- | Determines the value of @VerifiableType v@ is valid or not.
    verify :: Proxy v -- ^ Verifier type.
           -> (VerifiableType v) -- ^ Value to verify.
           -> Either (FailureHint v) (VerifiableType v) -- ^ @Right@ if valid, otherwise @Left@.

    -- | Convert the left value in the result of @verify@ into @ValidationError'@.
    verify' :: (AllFormattableArg (VerifierSpec v))
            => Proxy v -- ^ Verifier type.
            -> (VerifiableType v) -- ^ Value to verify.
            -> Either ValidationError' (VerifiableType v) -- ^ @Right@ if valid, otherwise @Left@.
    verify' p v = case verify p v of
                    Left h -> Left $ VerificationFailure p h
                    Right v' -> Right v'

    -- | Generates default error message.
    verificationFailure :: Proxy v -- ^ Verifier type.
                        -> [Pointer] -- ^ Pointer path where the error occurs.
                        -> FailureHint v -- ^ Hinting value to show the detail of the error.
                        -> String -- ^ Error message.
    verificationFailure _ ps hint = "Value at " ++ showPath ps True ++ " is invalid"

-- | Declares a method to compare two verifiers.
class EqVerifier v1 v2 where
    eqVerifier :: Proxy v1 -> Proxy v2 -> Bool

instance {-# OVERLAPPING #-} (Verifier v) => EqVerifier v v where
    eqVerifier p1 p2 = verifierSpec p1 == verifierSpec p2
instance (Verifier v1, Verifier v2) => EqVerifier v1 v2 where
    eqVerifier _ _ = False

-- | Hetero type lists representaing arguments.
data Args (args :: [*]) where
    -- | Empty list.
    ANil :: Args '[] -- ^ Empty list.
    -- | Prepend a value to the list.
    ACons :: a -- ^ Value to prepend.
          -> Args as -- ^ Arguments.
          -> Args (a ': as) -- ^ Extended arguments.

infixr 2 `ACons`

--type family ArgsConstraint (c :: * -> Constraint) (args :: [*]) :: Constraint where
--    ArgsConstraint ANil = ()
--    ArgsConstraint (a `ACons` as) = (c a, ArgsConstraint c as)

instance Eq (Args '[]) where
    (==) _ _ = True
instance (Eq a, Eq (Args as)) => Eq (Args (a ': as)) where
    (==) (a1 `ACons` as1) (a2 `ACons` as2) = a1 == a2 && as1 == as2

-- | Type-level function to generate type where types are applied to a function.
--
-- > Apply '[] A == A
-- > Apply '[A] B == A -> B
-- > Apply '[A, B] C == A -> Apply '[B] C == A -> B -> C
type family Apply (as :: [*]) (f :: *) :: *
type instance Apply '[] f = f
type instance Apply (a ': as) f = a -> Apply as f

showVerificationFailure :: forall bs v. (Verifier v, AllFormattableArg (VerifierSpec v))
                        => String
                        -> Proxy v
                        -> [Int]
                        -> String
                        -> String
showVerificationFailure key pv indexes format = printf' $ reverse formatters
    where
        formatters = let fmt = \i -> if i == 0 then formatArg key
                                               else argFormatter (verifierArgs pv) (i-1)
                     in map fmt indexes
        printf' :: (PrintfType r) => [FieldFormatter] -> r
        printf' [] = printf format
        printf' (f : fs) = printf' fs f

instance PrintfArg FieldFormatter where
    formatArg = id

class FormattableArg a where
    asFormatter :: a -> FieldFormatter

instance (PrintfArg a) => FormattableArg a where
    asFormatter = formatArg

type family AllFormattableArg (as :: [*]) :: Constraint where
    AllFormattableArg '[] = ()
    AllFormattableArg (a ': as) = (FormattableArg a, AllFormattableArg as)

argFormatter :: (AllFormattableArg as)
             => Args as
             -> Int
             -> FieldFormatter
argFormatter (a `ACons` as) 0 = formatArg $ asFormatter a
argFormatter (a `ACons` as) i = argFormatter as (i-1)

-- | Declares an operator to extract arguments of @Verifier@ by applying a function which accepts them in the same order.
--
-- This operator provides shorthand way to obtain @Verifier@ arguments in @verify@ or @verificationFailure@.
-- See an example below which shows the usage of @<-$@ in @verify@.
--
-- > data MinAppear (a :: Symbol) (b :: Nat)
-- >
-- > instance (KnownSymbol a, KnownNat b) => Verifier (MinAppear a b) where
-- >     type VerifiableType (MinAppear a b) = String
-- >     type VerifierSpec (MinAppear a b) = '[a, b]
-- >
-- >     verifierSpec _ = ( "MinAppear"
-- >                      , symbolVal (Proxy :: Proxy a) `ACons` natVal (Proxy :: Proxy b) `ACons` ANil
-- >                      )
-- >
-- >     verify _ v = 
-- >         let (target, minCount) = (,) <-$ verifierArgs (Proxy :: Proxy MinAppear) :: (String, Integer)
-- >         in if minCount <= length (filter (== target) words v) then Right v else Left ()
class ExtractArgs args r where
    (<-$) :: Apply args r -> Args args -> r

instance ExtractArgs '[] r where
    (<-$) f _ = f
instance (ExtractArgs as r) => ExtractArgs (a ': as) r where
    (<-$) f (a `ACons` as) = f a <-$ as

-- | Type operator to qualifying a type with verifiers.
data (:?) a (vs :: [*]) = SafeData a (Proxy vs)

instance (Eq a) => Eq (a :? vs) where
    (==) (SafeData a1 _) (SafeData a2 _) = a1 == a2
instance (Show a) => Show (a :? vs) where
    show (SafeData a _) = show a

-- | Returns verified value.
safeData :: (a :? vs) -- ^ Value of qualified type.
         -> a -- ^ The verified value of original type.
safeData (SafeData v _) = v

-- | Declares an operator to get the value from qualified type by record selector for original type.
--
-- > type A = A { f1 :: Int, f2 :: String }
-- > type A' = A :? '[V1, V2]
-- > f1 .! (a :: A') == f1 (safeData a)
class SafeAccess f t a where
    (.!) :: (f -> t)
         -> f
         -> a

infixl 2 .!

instance {-# OVERLAPPABLE #-} SafeAccess f a a where
    (.!) k v = k v
instance {-# OVERLAPS #-} SafeAccess f (a :? vs) a where
    (.!) k v = safeData $ k v
instance {-# OVERLAPPING #-} (Functor t) => SafeAccess f (t (a :? vs)) (t a) where
    (.!) k v = safeData <$> k v

-- | Constraint which claims all verifiers in @vs@ can be applied to @a@.
class AllVerifiable (vs :: [*]) a where
    -- | Apply verifiers in @vs@ to the valus of @a@ in order.
    verifyAll :: Proxy vs -- ^ List of verifier types.
              -> a -- ^ Value to verify.
              -> Either ValidationError' a -- ^ The result of verification.

instance AllVerifiable '[] a where
    verifyAll _ a = Right a
instance (Verifier v, VerifiableType v ~ a, AllVerifiable vs a, AllFormattableArg (VerifierSpec v)) => AllVerifiable (v ': vs) a where
    verifyAll _ a = verify' (Proxy :: Proxy v) a >>= verifyAll (Proxy :: Proxy vs)
