{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Thin layer over the @encoding@ package to create types compatible
-- with @type-encoding@.
--
-- ## Naming convention
-- 
-- @"r-enc:[encoding]"@ - where @"[encoding]"@ is String name used by the @encoding@ package 'DynEncoding'.
--
-- Example: @"r-enc:cyrillic"@
--
-- This module contains combinators which are polymorphic in the symbol annotation.
--
-- Since the enumeration of possible encodings is fixed, proper 'EncodeF', 'DecodeF', 'ToEncString', and 'FromEncString' instances
-- could be generated in the future.
module Data.TypedEncoding.Ext.Combinators.Restriction.Encoding where 

import           Data.TypedEncoding.Instances.Support (Enc, EncodeEx(..), UnexpectedDecodeErr, Algorithm)
import qualified Data.TypedEncoding.Instances.Support as Typed
import           Data.TypedEncoding.Instances.Support.Unsafe (implTranF)

import           Data.Encoding

import           GHC.TypeLits
import           Data.Proxy
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- $setup
-- >>> :set -XOverloadedStrings -XDataKinds -XTypeApplications
-- >>> import           Data.Functor.Identity

type family IsDynEnc (s :: Symbol) :: Bool where
    IsDynEnc s = Typed.AcceptEq ('Text "Not encoding restriction " ':<>: ShowType s ) (CmpSymbol (Typed.TakeUntil s ":") "r-enc")

type DynEnc s = (KnownSymbol s, IsDynEnc s ~ 'True)

instance (DynEnc s, Algorithm s "r-enc") => Typed.Encode (Either EncodeEx) s "r-enc" c B.ByteString where
    encoding = encDynB

instance (DynEnc s, Algorithm s "r-enc") => Typed.Encode (Either EncodeEx) s "r-enc" c BL.ByteString where
    encoding = encDynBL

instance (KnownSymbol s, Typed.Restriction s, Algorithm s "r-enc", Applicative f) => Typed.Decode f s "r-enc" c str where
    decoding = Typed.decAnyR_

instance (KnownSymbol s , DynEnc s, Algorithm s "r-enc", Typed.RecreateErr f, Applicative f) => Typed.Validate f s "r-enc" c B.ByteString where
    validation = Typed.validFromEnc' @"r-enc" encDynB

instance (DynEnc s, Algorithm s "r-enc") => Typed.ToEncString (Either EncodeEx) s "r-enc" String B.ByteString where
  toEncF = toDynEncB

instance (DynEnc s, Algorithm s "r-enc") => Typed.ToEncString (Either EncodeEx) s "r-enc" String BL.ByteString where
  toEncF = toDynEncBL

instance (UnexpectedDecodeErr f, Monad f, DynEnc s, Algorithm s "r-enc") => Typed.FromEncString f s "r-enc" String B.ByteString where
  fromEncF = fromDynEncB

instance (UnexpectedDecodeErr f, Monad f, DynEnc s, Algorithm s "r-enc") => Typed.FromEncString f s "r-enc" String BL.ByteString where
  fromEncF = fromDynEncBL



-- | Gives type safety over existence of DynEncoding
getDynEncoding :: forall s xs c str. (DynEnc s) => Enc (s ': xs) c str -> DynEncoding
getDynEncoding _ = encodingFromString nm 
  where 
      p = Proxy :: Proxy s
      nm = L.drop 6 . symbolVal $ p

encDynB :: forall s c .
              (
                DynEnc s
                , Algorithm s "r-enc"
               ) => 
               Typed.Encoding (Either EncodeEx) s "r-enc" c B.ByteString
encDynB = Typed._implEncodingEncodeEx @s  (Typed.verifyDynEnc (Proxy :: Proxy s) verifyDynEncoding decodeStrictByteStringExplicit)              

-- | 
--
-- @"r-enc:"@ restriction encoding of @ByteString@ 
--
-- @"r-" encoding simply verifies that restriction is correct.
encDynBL :: forall s xs c .
              (
               DynEnc s
               , Algorithm s "r-enc"
              ) => 
              Typed.Encoding (Either EncodeEx) s "r-enc" c BL.ByteString
encDynBL = Typed._implEncodingEncodeEx @s (Typed.verifyDynEnc (Proxy :: Proxy s) verifyDynEncoding decodeLazyByteStringExplicit)              

-- |
-- 
-- >>> toDynEncB @"r-enc:cyrillic" "Статья"
-- Right (UnsafeMkEnc Proxy () "\193\226\208\226\236\239")
--
-- >>> "Статья"
-- "\1057\1090\1072\1090\1100\1103"
toDynEncB :: forall s .
             (
                DynEnc s
               , Algorithm s "r-enc"
               ) => 
              String -> Either EncodeEx (Enc '[s] () B.ByteString)
toDynEncB s = 
    do 
        enc <- Typed.asEncodeEx p . verifyDynEncoding $ p
        fmap (Typed.unsafeSetPayload ()) . Typed.asEncodeEx p . encodeStrictByteStringExplicit enc $ s
        where 
          p = Proxy :: Proxy s


-- | 
-- Converts 'String' to some @Enc '["r-enc:..."] () BL.ByteString@ type
-- by actually encoding characters in the String into correct byte layout.
toDynEncBL :: forall s .
             (
                DynEnc s
                , Algorithm s "r-enc"
                ) => 
              String -> Either EncodeEx (Enc '[s] () BL.ByteString)
toDynEncBL s = 
    do 
        enc <- Typed.asEncodeEx p $ verifyDynEncoding p
        fmap (Typed.unsafeSetPayload ()) . Typed.asEncodeEx p . encodeLazyByteStringExplicit enc $ s
        where 
          p = Proxy :: Proxy s


-- |
--
-- >>> fromDynEncB @"r-enc:cyrillic" @Identity (Typed.unsafeSetPayload () "\193\226\208\226\236\239")
-- Identity "\1057\1090\1072\1090\1100\1103"
fromDynEncB :: forall s f .
                     (UnexpectedDecodeErr f
                     , Monad f
                     , DynEnc s
                     , Algorithm s "r-enc"
                    ) => 
                     Enc '[s] () B.ByteString -> f String
fromDynEncB x = 
  do 
    enc <- Typed.asUnexpected @s . verifyDynEncoding $ p
    Typed.asUnexpected @s . decodeStrictByteStringExplicit enc . Typed.getPayload $ x
  where p = Proxy :: Proxy s


fromDynEncBL :: forall s f .
                     (UnexpectedDecodeErr f
                     , Monad f
                     , DynEnc s
                     , Algorithm s "r-enc"
                    ) => 
                     Enc '[s] () BL.ByteString -> f String
fromDynEncBL x = 
  do 
    enc <- Typed.asUnexpected @s . verifyDynEncoding $ p
    Typed.asUnexpected @s . decodeLazyByteStringExplicit enc . Typed.getPayload $ x
  where p = Proxy :: Proxy s

-- * private

verifyDynEncoding :: (KnownSymbol s, DynEnc s) => Proxy s -> Either String DynEncoding
verifyDynEncoding p = explainMaybe ("Invalid encoding " ++ nm) . encodingFromStringExplicit $ nm 
  where 
      nm = L.drop 6 . symbolVal $ p
      explainMaybe _ (Just x) = Right x
      explainMaybe msg Nothing = Left msg




