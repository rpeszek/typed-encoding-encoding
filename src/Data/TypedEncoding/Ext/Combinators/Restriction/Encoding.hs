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
-- @"r-lib/encoding:[encoding]"@ - where @"[encoding]"@ is String name used by the 'Encoding.DynEncoding' in /encoding/ package.
--
-- Example: @"r-lib/encoding:cyrillic"@
--
-- This module contains combinators which are polymorphic in the symbol annotation.
--
-- Sadly, /encoding/ library does not provide ways to encode and decode from the popular /Text/ type (/text/ package).  
--
-- Key instances defined here are 'Typed.ToEncString' and 'Typed.FromEncString'.
-- These allow to create encoded @ByteString@ from a String (@ToEncString@) and decode @ByteString@ back (@ToEncString@).
--
-- Other instances are less interesting and provide validation facilities.
--
-- It should be possible to create more efficient versions of 'Typed.Encode' in the future, which do not use String decoding under the hood.
module Data.TypedEncoding.Ext.Combinators.Restriction.Encoding where 

import qualified Data.TypedEncoding.Instances.Support as Typed

import qualified Data.Encoding as Encoding

import           GHC.TypeLits
import           Data.Proxy
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- $setup
-- >>> :set -XOverloadedStrings -XDataKinds -XTypeApplications -XFlexibleContexts
-- >>> import           Data.Functor.Identity
-- >>> import qualified Data.TypedEncoding as Usage


type family IsDynEnc (s :: Symbol) :: Bool where
    IsDynEnc s = Typed.AcceptEq ('Text "Not encoding restriction " ':<>: ShowType s ) (CmpSymbol (Typed.TakeUntil s ":") "r-lib/encoding")

type DynEnc s = (KnownSymbol s, IsDynEnc s ~ 'True)

-- |
-- 
-- >>> Usage.displ <$> Usage.toEncStringF' @"r-lib/encoding" @"r-lib/encoding:cyrillic" @(Either Usage.EncodeEx) @String @B.ByteString "Статья"
-- Right "Enc '[r-lib/encoding:cyrillic] () (ByteString \193\226\208\226\236\239)"

instance (DynEnc s, Typed.Algorithm s "r-lib/encoding") => Typed.ToEncString (Either Typed.EncodeEx) s "r-lib/encoding" String B.ByteString where
  toEncF = toDynEncB

instance (DynEnc s, Typed.Algorithm s "r-lib/encoding") => Typed.ToEncString (Either Typed.EncodeEx) s "r-lib/encoding" String BL.ByteString where
  toEncF = toDynEncBL

-- |
-- 
-- >>> Usage.displ <$> Usage.fromEncStringF' @"r-lib/encoding" @"r-lib/encoding:cyrillic" @(Either Usage.UnexpectedDecodeEx) @String @B.ByteString (Typed.unsafeSetPayload () "\193\226\208\226\236\239")
-- Right "(String \1057\1090\1072\1090\1100\1103)"
instance (Typed.UnexpectedDecodeErr f, Monad f, DynEnc s, Typed.Algorithm s "r-lib/encoding") => Typed.FromEncString f s "r-lib/encoding" String B.ByteString where
  fromEncF = fromDynEncB

instance (Typed.UnexpectedDecodeErr f, Monad f, DynEnc s, Typed.Algorithm s "r-lib/encoding") => Typed.FromEncString f s "r-lib/encoding" String BL.ByteString where
  fromEncF = fromDynEncBL


-- |
-- These are just verifications of byte layouts
-- 
-- >>> fmap Usage.displ . Usage.encodeFAll' @'["r-lib/encoding"] @'["r-lib/encoding:cyrillic"] @(Either Usage.EncodeEx) @() @B.ByteString . Usage.toEncoding () $ "\193\226\208\226\236\255"
-- Right "Enc '[r-lib/encoding:cyrillic] () (ByteString \193\226\208\226\236\255)"
--
-- >>> fmap Usage.displ . Usage.encodeFAll' @'["r-lib/encoding"] @'["r-lib/encoding:greek"] @(Either Usage.EncodeEx) @() @B.ByteString . Usage.toEncoding () $ "\193\226\208\226\236\255"
-- Left (EncodeEx "r-lib/encoding:greek" (IllegalCharacter 255))
instance (DynEnc s, Typed.Algorithm s "r-lib/encoding") => Typed.Encode (Either Typed.EncodeEx) s "r-lib/encoding" c B.ByteString where
    encoding = encDynB

instance (DynEnc s, Typed.Algorithm s "r-lib/encoding") => Typed.Encode (Either Typed.EncodeEx) s "r-lib/encoding" c BL.ByteString where
    encoding = encDynBL

instance (KnownSymbol s, Typed.Restriction s, Typed.Algorithm s "r-lib/encoding", Applicative f) => Typed.Decode f s "r-lib/encoding" c str where
    decoding = Typed.decAnyR_

-- |
-- For "r-" encodings this is the same as @Encode@.
--
-- >>> fmap Usage.displ . Usage.recreateFAll' @'["r-lib/encoding"] @'["r-lib/encoding:greek"] @(Either Usage.RecreateEx) @() @B.ByteString  . Usage.toEncoding () $ "\193\226\208\226\236\255"
-- Left (RecreateEx "r-lib/encoding:greek" (IllegalCharacter 255))
instance (KnownSymbol s , DynEnc s, Typed.Algorithm s "r-lib/encoding", Typed.RecreateErr f, Applicative f) => Typed.Validate f s "r-lib/encoding" c B.ByteString where
    validation = Typed.validFromEnc' @"r-lib/encoding" encDynB


-- * Combinators

-- | Gives type safety over existence of Encoding.DynEncoding
getDynEncoding :: forall s xs c str. (DynEnc s) => Typed.Enc (s ': xs) c str -> Encoding.DynEncoding
getDynEncoding _ = Encoding.encodingFromString nm 
  where 
      p = Proxy :: Proxy s
      nm = L.drop 6 . symbolVal $ p

encDynB :: forall s c .
              (
                DynEnc s
                , Typed.Algorithm s "r-lib/encoding"
               ) => 
               Typed.Encoding (Either Typed.EncodeEx) s "r-lib/encoding" c B.ByteString
encDynB = Typed._implEncodingEncodeEx @s  (Typed.verifyDynEnc (Proxy :: Proxy s) verifyDynEncoding Encoding.decodeStrictByteStringExplicit)              

encDynBL :: forall s xs c .
              (
               DynEnc s
               , Typed.Algorithm s "r-lib/encoding"
              ) => 
              Typed.Encoding (Either Typed.EncodeEx) s "r-lib/encoding" c BL.ByteString
encDynBL = Typed._implEncodingEncodeEx @s (Typed.verifyDynEnc (Proxy :: Proxy s) verifyDynEncoding Encoding.decodeLazyByteStringExplicit)              

-- |
-- 
-- >>> toDynEncB @"r-lib/encoding:cyrillic" "Статья"
-- Right (UnsafeMkEnc Proxy () "\193\226\208\226\236\239")
--
-- >>> Typed.displ <$> toDynEncB @"r-lib/encoding:cyrillic" "Статья"
-- Right "Enc '[r-lib/encoding:cyrillic] () (ByteString \193\226\208\226\236\239)"
-- 
-- >>> "Статья"
-- "\1057\1090\1072\1090\1100\1103"
--
-- >>> toDynEncB @"r-lib/encoding:ascii" "Статья"
-- Left (EncodeEx "r-lib/encoding:ascii" (HasNoRepresentation '\1057'))
toDynEncB :: forall s .
             (
                DynEnc s
               , Typed.Algorithm s "r-lib/encoding"
               ) => 
              String -> Either Typed.EncodeEx (Typed.Enc '[s] () B.ByteString)
toDynEncB s = 
    do 
        enc <- Typed.asEncodeEx p . verifyDynEncoding $ p
        fmap (Typed.unsafeSetPayload ()) . Typed.asEncodeEx p . Encoding.encodeStrictByteStringExplicit enc $ s
        where 
          p = Proxy :: Proxy s


-- | 
-- Converts 'String' to some @Enc '["r-lib/encoding:..."] () BL.ByteString@ type
-- by actually encoding characters in the String into correct byte layout.
toDynEncBL :: forall s .
             (
                DynEnc s
                , Typed.Algorithm s "r-lib/encoding"
                ) => 
              String -> Either Typed.EncodeEx (Typed.Enc '[s] () BL.ByteString)
toDynEncBL s = 
    do 
        enc <- Typed.asEncodeEx p $ verifyDynEncoding p
        fmap (Typed.unsafeSetPayload ()) . Typed.asEncodeEx p . Encoding.encodeLazyByteStringExplicit enc $ s
        where 
          p = Proxy :: Proxy s


-- |
--
-- >>> fromDynEncB @"r-lib/encoding:cyrillic" @Identity (Typed.unsafeSetPayload () "\193\226\208\226\236\239")
-- Identity "\1057\1090\1072\1090\1100\1103"
--
-- >>> fromDynEncB @"r-lib/encoding:ascii" @Identity (Typed.unsafeSetPayload () "\193\226\208\226\236\239")
-- Identity "\193\226\208\226\236\239"
fromDynEncB :: forall s f .
                     (Typed.UnexpectedDecodeErr f
                     , Monad f
                     , DynEnc s
                     , Typed.Algorithm s "r-lib/encoding"
                    ) => 
                     Typed.Enc '[s] () B.ByteString -> f String
fromDynEncB x = 
  do 
    enc <- Typed.asUnexpected @s . verifyDynEncoding $ p
    Typed.asUnexpected @s . Encoding.decodeStrictByteStringExplicit enc . Typed.getPayload $ x
  where p = Proxy :: Proxy s


fromDynEncBL :: forall s f .
                     (Typed.UnexpectedDecodeErr f
                     , Monad f
                     , DynEnc s
                     , Typed.Algorithm s "r-lib/encoding"
                    ) => 
                     Typed.Enc '[s] () BL.ByteString -> f String
fromDynEncBL x = 
  do 
    enc <- Typed.asUnexpected @s . verifyDynEncoding $ p
    Typed.asUnexpected @s . Encoding.decodeLazyByteStringExplicit enc . Typed.getPayload $ x
  where p = Proxy :: Proxy s

-- * Private

verifyDynEncoding :: (KnownSymbol s, DynEnc s) => Proxy s -> Either String Encoding.DynEncoding
verifyDynEncoding p = explainMaybe ("Invalid encoding " ++ nm) . Encoding.encodingFromStringExplicit $ nm 
  where 
      nm = L.drop 15 . symbolVal $ p
      explainMaybe _ (Just x) = Right x
      explainMaybe msg Nothing = Left msg



