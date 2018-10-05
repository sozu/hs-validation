{-# LANGUAGE TemplateHaskell #-}
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
{-# LANGUAGE TupleSections #-}

module Data.Validation.TH where

import GHC.Generics
import Control.Applicative
import Control.Monad
import Control.Monad.Except (catchError, throwError)
import Data.Proxy
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.List as L
import Language.Haskell.TH
import qualified Data.Text as T
import Data.Maybe (maybe, catMaybes)
import Data.Either (either)
import Data.Aeson as J
import qualified Data.Aeson.BetterErrors as JB
import Data.Aeson.Types
import Web.FormUrlEncoded as F
import Web.HttpApiData
import Data.Validation.Validation
import Data.Validation.Aeson
import Data.Validation.Http

-- ----------------------------------------------------------------
-- TH function to generate types used in validation.
-- ----------------------------------------------------------------

{- | Declares new data type which has equivalent fields to given type.

    The name of the type and its fields are determined by appending @'@ to the origina name.
    In the new type, type of each field is converted by applying @F@ in following manners.

    - @Maybe a@ is changed to @F (Maybe a)@.
    - @[a]@ is changed to @F [F a]@.
    - Other types are simply applied by @F@, that is, @a@ is changed to @F a@.
    - If the original type @A@ is the type involved in the arguments of the same invocation of this function,
      @A'@ which is the new type of @A@ is used instead.

    Besides, several instances are defined to enable validation from data in the form of JSON or HTTP form.
    Next code is the example of generated code.
    (The implementations of @Validatable@ are omitted because they include elaborate part taken to deal with DuplicateRecordFields.)

    > data A = A { a1 :: Int, a2 :: [Int], a3 :: Maybe Int }
    > data B = B { b1 :: A, b2 :: [A], b3 :: Maybe A }
    >
    > -- $(validatable [''A, ''B]) generates code below.
    >
    > data A' = A' { a1' :: F Int, a2' :: F [F Int], a3' :: F (Maybe Int) } deriving (Generic)
    > data B' = B' { b1' :: F A', b2' :: F [F A'], b3' :: F (Maybe A') } deriving (Generic)
    >
    > instance FromJSONBetterErrors A' where
    >     fromJSONBetterErrors = A' <$> asField (Proxy :: Proxy (F Int)) (KeyPointer "a1")
    >                               <*> asField (Proxy :: Proxy (F [F Int])) (KeyPointer "a2")
    >                               <*> asField (Proxy :: Proxy (F (Maybe Int))) (KeyPointer "a3")
    > instance FromJSONBetterErrors B' where
    >     ...
    >
    > instance FromForm A' where
    >     fromForm f = A' <$> asFormField (Proxy :: Proxy (F Int)) "a1" f
    >                     <*> asFormField (Proxy :: Proxy (F [F Int])) "a2" f
    >                     <*> asFormField (Proxy :: Proxy (F (Maybe Int))) "a3" f
    > instance FromForm B' where
    >     ...
    >
    > instance AsType A' where
    >     asType _ = fromJSONBetterErrors
    > instance AsType B' where
    >     ...
    >
    > instance AsFormField A' where
    >     asFormField _ _ _ = Left (T.pack "Nested type is not available as a form field")
    > instance AsFormField B' where
    >     ...
    >
    > instance Validatable A' where
    >      type ValidationTarget A' = A
    >      validate v = A <$> ...
    >      errors p v = ...
    >
    > instance Validatable B' where
    >      type ValidationTarget B' = B
    >      validate v = B <$> ...
    >      errors p v = ...

    TODO: should generate instance of FromHttpApiData instead of FromForm ?
-}
validatable :: [Name]
            -> Q [Dec]
validatable ns = concat <$> mapM conv ns
    where
        conv :: Name -> Q [Dec]
        conv name = do
            TyConI (DataD _ _ tvs kind (c@(RecC cn recs):_) drvs) <- reify name
            let fields = map (\(rn, _, rt) -> (genFieldName rn, rt)) recs
            let vn = mkName "v"
            let pn = mkName "p"
            let c' = constructorOf n' c
            dJson <- deriveBetterFromJSON n' c'
            dForm <- deriveFromForm n' c'
            dValidatable <- [d|
                    instance Validatable $(conT n') where
                        type ValidationTarget $(conT n') = $(conT name)
                        validate $(varP vn) = $(validateFor cn vn fields)
                        errors $(varP pn) $(varP vn) = $(errorsFor pn vn fields)
                |]
            dParse <- [d|
                    instance AsType $(conT n') where
                        asType _ = fromJSONBetterErrors
                    instance AsFormField $(conT n') where
                        --asFormField _ _ _ = Left $ T.pack "Nested type is not available as a form field."
                        asFormField _ _ _ = Left $ ErrorString "Nested type is not available as a form field."
                |]
            return $ concat [[
                DataD [] n' [] Nothing [constructorOf n' c] [DerivClause Nothing [(ConT ''Generic)]]
              , dJson
              , dForm
              ], dValidatable, dParse]
            where
                -- Name of validatable data type.
                n' = genTypeName name
                -- Generates new type name.
                genTypeName n = mkName $ nameBase n ++ "'"
                -- Generates new field (wrapped by @F@) name.
                genFieldName n = mkName $ nameBase n ++ "'"

                -- Applies another expression with (<*>), namely, app [| x |] [| y |] == [| x <*> y |].
                app :: Exp -> Exp -> Exp
                app x y = InfixE (Just x) (VarE '(<*>)) (Just y)

                -- Get field type of given type.
                -- Validatable type is replaced with its @Validatable@ version (qualified with @'@).
                fieldTypeOf :: Type -> FieldType
                fieldTypeOf t@(ConT n)
                    | n `L.elem` ns = ValidatableScalar (ConT n) (ConT $ genTypeName n)
                    | otherwise = NormalScalar t
                fieldTypeOf (AppT ListT t@(ConT n))
                    | n `L.elem` ns = ValidatableList (ConT n) (ConT $ genTypeName n)
                    | otherwise = NormalList t
                fieldTypeOf t@(AppT (AppT (ConT m) k) (ConT v))
                    | nameBase m == "Map" && v `L.elem` ns = ValidatableMap k (ConT v) (ConT $ genTypeName v)
                    | nameBase m == "Map" = NormalMap k (ConT v)
                fieldTypeOf t@(AppT (ConT m) (ConT n))
                    | nameBase m == "Maybe" && n `L.elem` ns = ValidatableMaybe (ConT n) (ConT $ genTypeName n)
                    | otherwise = NormalScalar t
                fieldTypeOf t = NormalScalar t

                -- Generates an implementation of @validate@
                validateFor :: Name -> Name -> [(Name, Type)] -> ExpQ
                validateFor cn vn fields = applicativeCon cn <$> (mapM (appValueExpQ vn) fields)

                -- Generates an implementation of @errors@
                errorsFor :: Name -> Name -> [(Name, Type)] -> ExpQ
                errorsFor pn vn fields = appE (varE 'concat) (listE $ map (appCauseExpQ pn vn) fields)

                -- Generates expression obtaining valid data from a field.
                -- Normal type:              [| value                ((f1 :: A' -> F String)          (v :: A')) |]
                -- Validatable type:         [| (value >=> validate) ((f1 :: B' -> F A')              (v :: B')) |]
                -- List of normal type:      [| value                ((f1 :: A' -> F [F String])      (v :: A')) >>= sequence . map value |]
                -- List of validatable type: [| value                ((f1 :: B' -> F [F A'])          (v :: B')) >>= sequence . map (value >=> validate) |]
                -- Map of normal type:       [| value                ((f1 :: A' -> F (Map k (F Int))) (v :: A')) >>= validateMap Just |]
                -- Map of validatable type:  [| value                ((f1 :: B' -> F (Map k (F A')))  (v :: B')) >>= validateMap validate |]
                -- Maybe of validatable type:[| value                ((f1 :: B' -> F (Maybe A'))      (v :: B')) >>= id >>= return . validate |]
                appValueExpQ :: Name -> (Name, Type) -> ExpQ
                appValueExpQ vn (fn, ft) = do
                    let sigV = [| $(varE vn) :: $(conT n') |]
                    let sigF t = [| $(varE fn) :: $(conT n') -> F $(return t) |]
                    let fListT t = AppT ListT (AppT (ConT ''F) t)
                    let fMapT k v = AppT (AppT (ConT ''M.Map) k) (AppT (ConT ''F) v)
                    let maybeT t = AppT (ConT ''Maybe) t
                    case fieldTypeOf ft of
                        NormalScalar t        -> [| value                ($(sigF t) $(sigV)) |]
                        ValidatableScalar _ t -> [| (value >=> validate) ($(sigF t) $(sigV)) |]
                        NormalList t          -> [| value                ($(sigF $ fListT t) $(sigV)) >>= sequence . map value |]
                        ValidatableList _ t   -> [| value                ($(sigF $ fListT t) $(sigV)) >>= sequence . map (value >=> validate) |]
                        NormalMap k v         -> [| value                ($(sigF $ fMapT k v) $(sigV)) >>= validateMap Just |]
                        ValidatableMap k _ v  -> [| value                ($(sigF $ fMapT k v) $(sigV)) >>= validateMap validate |]
                        ValidatableMaybe _ t  -> [| value                ($(sigF $ maybeT t) $(sigV)) >>= id >>= return . validate |]

                -- Generates expression obtaining list of errors from a field.
                -- f = (f1 :: A' -> F a) (v :: A')
                -- errX :: F x -> Maybe [ValidationError]
                -- errN p (f :: F a) = (:[]) <$> (p !@) <$> (cause f)
                -- errV p (f :: F A') = errN p f <|> errors p <$> (value f)
                -- errI p (i, f) err = err (p ++ [IndexPointer i]) f
                -- errK p (k, f) err = err (p ++ [KeyPointer k]) f
                -- errNS p (f :: F [F a]) = errN p f <|> (value f >>= return . concat . catMaybes . map errNI . zip [0..])
                -- errVS p (f :: F [F A']) = errN p f <|> (value f >>= return . concat . catMaybes . map errVI . zip [0..])
                -- errNM p (f :: F (Map k (F a))) = errN p f <|> (value f >>= return . concat . catMaybes . map errNK . toList)
                -- errMB p (f :: F (Maybe A')) = errN p f <|> (value f >>= id >>= return . errors p)
                -- maybe [] id (errX f)
                appCauseExpQ :: Name -> Name -> (Name, Type) -> ExpQ
                appCauseExpQ pn vn (fn, ft) = do
                    let f t = [| ($(varE fn) :: $(conT n') -> F $(return t)) ($(varE vn) :: $(conT n')) |]
                    let errN = [| (\p f -> (:[]) . (p !@) <$> cause f) |]
                    let errV t = [| (\p f -> $(errN) p f <|> (errors p <$> value f)) |]
                    let errI p err = map (\(i, f) -> err (p ++ [IndexPointer i]) f) . zip [0..]
                    let fListT t = AppT ListT (AppT (ConT ''F) t)
                    let fMapT k v = AppT (AppT (ConT ''M.Map) k) (AppT (ConT ''F) v)
                    let path = [| $(varE pn) ++ [KeyPointer $ stripSuffix (nameBase fn)] |]
                    let errs = case fieldTypeOf ft of
                            NormalScalar t         -> [| let f' = $(f t) in $(errN) $(path) f' |]
                            ValidatableScalar t' t -> [| let f' = $(f t) in $(errV t') $(path) f' |]
                            NormalList t           -> [|
                                    let f' = $(f $ fListT t)
                                    in $(errN) $(path) f' <|> (value f' >>=
                                            return . concat . catMaybes . map (\(i, f) -> $(errN) ($(path) ++ [IndexPointer i]) f) . zip [0..]
                                        )
                                |]
                            ValidatableList t' t   -> [|
                                    let f' = $(f $ fListT t)
                                    in $(errN) $(path) f' <|> (value f' >>=
                                            return . concat . catMaybes . map (\(i, f) -> $(errV t') ($(path) ++ [IndexPointer i]) f) . zip [0..]
                                        )
                                |]
                            NormalMap k v          -> [|
                                    let f' = $(f $ fMapT k v)
                                    in $(errN) $(path) f' <|> (value f' >>=
                                            return . concat . catMaybes . map (\(k, f) -> $(errN) ($(path) ++ [KeyPointer k]) f) . fromMapToList'
                                        )
                                |]
                            ValidatableMap k v' v  -> [|
                                    let f' = $(f $ fMapT k v)
                                    in $(errN) $(path) f' <|> (value f' >>=
                                            return . concat . catMaybes . map (\(k, f) -> $(errV v') ($(path) ++ [KeyPointer k]) f) . fromMapToList'
                                        )
                                |]
                            ValidatableMaybe t' t  -> let mt = AppT (ConT ''Maybe) t in [|
                                    let f' = $(f mt)
                                    in $(errN) $(path) f' <|> (value f' >>= id >>= return . errors $(path))
                                |]
                    [| maybe [] id $(errs) |]

                -- Generates data constructor by wrapping all types of fields with @F@.
                constructorOf :: Name -> Con -> Con
                constructorOf cn (RecC _ recs) = RecC cn (map (\(rn, bang, ft) -> (genFieldName rn, bang, fieldType ft)) recs)
                    where
                        fieldType t = case fieldTypeOf t of
                                        NormalScalar t        -> AppT (ConT ''F) t
                                        ValidatableScalar _ t -> AppT (ConT ''F) t
                                        NormalList t          -> AppT (ConT ''F) (AppT ListT (AppT (ConT ''F) t))
                                        ValidatableList _ t   -> AppT (ConT ''F) (AppT ListT (AppT (ConT ''F) t))
                                        NormalMap k v         -> AppT (ConT ''F) (AppT (AppT (ConT ''M.Map) k) (AppT (ConT ''F) v))
                                        ValidatableMap k _ v  -> AppT (ConT ''F) (AppT (AppT (ConT ''M.Map) k) (AppT (ConT ''F) v))
                                        ValidatableMaybe _ t  -> AppT (ConT ''F) (AppT (ConT ''Maybe) t)

--jsonOptions = defaultOptions { J.fieldLabelModifier = stripSuffix, J.omitNothingFields = True }
--formOptions = defaultFormOptions { F.fieldLabelModifier = stripSuffix }

fromListToMap' = M.fromDistinctAscList
fromMapToList' = M.toList

validateMap :: (v -> Maybe u) -> M.Map k (F v) -> Maybe (M.Map k u)
validateMap f m = fromListToMap' <$> mapM (\(k, fv) -> (k,) <$> (value >=> f) fv) (fromMapToList' m)

stripSuffix :: String -> String
stripSuffix = reverse . strip . reverse
    where
        strip ('\'':cs) = cs
        strip cs = cs

data FieldType = NormalScalar Type
               | NormalList Type
               | NormalMap Type Type
               | ValidatableScalar Type Type
               | ValidatableList Type Type
               | ValidatableMap Type Type Type
               | ValidatableMaybe Type Type

class FromJSONBetterErrors a where
    fromJSONBetterErrors :: JB.Parse ValidationError' a

instance {-# OVERLAPPABLE #-} (FromJSONBetterErrors a) => FromJSON a where
    parseJSON = JB.toAesonParser (\e -> T.pack $ show e) fromJSONBetterErrors

-- | Generates declaration of function to parse given type in aeson-better-errors style.
--
--  > data A = A { f1 :: String, f2 :: Maybe Int }
--  > $(deriveBetterFromJSON ''A)
--  >
--  > instance FromJSONBetterErrors A where
--  >     fromJSONBetterErrors = A <$> asField (Proxy :: Proxy String) (KeyPointer "f1")
--  >                              <*> asField (Proxy :: Proxy (Maybe Int)) (KeyPointer "f2")
deriveBetterFromJSON :: Name
                     -> Con
                     -> DecQ
deriveBetterFromJSON n c@(RecC cn recs) = do
    --TyConI (DataD _ _ _ _ (RecC cn recs:_) _) <- reify n
    let fjbe = funD 'fromJSONBetterErrors [clause [] (normalB $ asFields cn recs) []]
    instanceD (cxt []) (appT (conT ''FromJSONBetterErrors) (conT n)) [fjbe]
    where
        asFields :: Name -> [(Name, Bang, Type)] -> ExpQ
        asFields cn rs = do
            recs <- mapM (\(rn, _, rt) -> [| asField (Proxy :: Proxy $(return rt)) (KeyPointer $ stripSuffix $ show rn) |]) rs
            return $ applicativeCon cn recs

-- | Generates instance of @FromForm@ for given type.
--
--  > data A = A { f1 :: String, f2 :: Maybe Int }
--  > $(deriveFromForm ''A)
--  >
--  > instance FromForm A where
--  >     fromForm f = A <$> asFormField (Proxy :: Proxy String) "f1" f
--  >                    <*> asFormField (Proxy :: Proxy (Maybe Int)) "f2" f
deriveFromForm :: Name
               -> Con
               -> DecQ
deriveFromForm n c@(RecC cn recs) = do
    let ff = funD 'fromForm [clause [varP $ mkName "f"] (normalB $ asFields cn recs) []]
    instanceD (cxt []) (appT (conT ''FromForm) (conT n)) [ff]
    where
        asFields :: Name -> [(Name, Bang, Type)] -> ExpQ
        asFields cn rs = do
            recs <- mapM (\(rn, _, rt) -> [| vett' (asFormField (Proxy :: Proxy $(return rt)) (stripSuffix $ show rn) f) |]) rs
            return $ applicativeCon cn recs

-- ----------------------------------------------------------------
-- Helper functions
-- ----------------------------------------------------------------

-- | Generate an expression applying data constructo to expressions concatenated by @<*>@.
--
-- > applicativeCon 'A [exp1, exp2, exp3] == runQ [| A' <$> $(exp1) <*> $(exp2) <*> $(exp3) |]
applicativeCon :: Name -> [Exp] -> Exp
applicativeCon cn [] = ConE cn
applicativeCon cn (a:as) = applicativeAcc (InfixE (Just $ ConE cn) (VarE '(<$>)) (Just a)) as

-- | Generate an expression concatenating expressions by @<*>@.
--
-- > applicativeAcc exp1 [exp2, exp3] == runQ [| $(exp1) <*> $(exp2) <*> $(exp3) |]
applicativeAcc :: Exp -> [Exp] -> Exp
applicativeAcc b [] = b
applicativeAcc b (e:es) = applicativeAcc (InfixE (Just b) (VarE '(<*>)) (Just e)) es