{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Validation.Http where

import Control.Applicative
import Control.Monad
import Control.Monad.Except (catchError, throwError)
import Data.Proxy
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.List as L
import Language.Haskell.TH
import qualified Data.Text as T
import Data.Aeson as J
import Web.FormUrlEncoded as F
import Web.HttpApiData
import Data.Validation.Validation

-- ----------------------------------------------------------------
-- For HTTP form 
-- ----------------------------------------------------------------

-- | Declares a method to parse HTTP form into field value.
class AsFormField a where
    -- | Parses a value in a form into field value of type @a@.
    asFormField :: Proxy a -- ^ Type specifier.
                -> String -- ^ Form key.
                -> Form -- ^ HTTP form.
                -> Either ValidationError' a -- ^ Parsed value or error message.

instance FromHttpApiData Object where
    parseUrlPiece _ = Left $ T.pack "Json object type can not be parsed by form data"

ttve' :: Either T.Text a
      -> Either ValidationError' a
ttve' lr = either (Left . ErrorString . T.unpack) return lr

vett' :: Either ValidationError' a
      -> Either T.Text a
vett' lr = either (Left . T.pack . show) return lr

instance {-# OVERLAPPABLE #-} (FromHttpApiData a) => AsFormField a where
    asFormField _ n f = ttve' $ F.parseUnique (T.pack n) f
instance {-# OVERLAPPING #-} AsFormField String where
    asFormField _ n f = ttve' $ F.parseUnique (T.pack n) f

instance {-# OVERLAPS #-} (AsFormField a, AllVerifiable vs a) => AsFormField (a :? vs) where
    asFormField _ n f = case asFormField (Proxy :: Proxy a) n f of
            Right v -> case verifyAll (Proxy :: Proxy vs) v of
                        Right v' -> Right $ SafeData v (Proxy :: Proxy vs)
                        Left e -> Left e
            Left e -> Left e

instance {-# OVERLAPS #-} (AsFormField a) => AsFormField (Maybe a) where
    asFormField _ n f = ttve' (F.lookupMaybe (T.pack n) f) >>= \v' -> 
        case v' of
            Nothing -> return Nothing
            Just v -> Just <$> asFormField (Proxy :: Proxy a) n f

instance {-# OVERLAPS #-} (AsFormField a) => AsFormField [a] where
    -- FIXME index information is lost.
    asFormField _ n f = mapM (\p -> asFormField (Proxy :: Proxy a) n $ F.toForm [(n, p)]) params
        where
            params = F.lookupAll (T.pack n) f

instance {-# OVERLAPPABLE #-} (AsFormField a) => AsFormField (F a) where
    asFormField _ n f = return $ case asFormField (Proxy :: Proxy a) n f of
                                    Right v -> F (Just v) EmptyValidatable Nothing
                                    Left e -> F Nothing EmptyValidatable (Just e)
