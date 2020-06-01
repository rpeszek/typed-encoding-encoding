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

-- | 
-- Contains type save equivalents of the following functions defined in /encoding/:
--
-- * 'Encoding.encodeStrictByteStringExplicit'
-- * 'Encoding.encodeLazyByteStringExplicit'
-- * 'Encoding.encString'
-- * 'Encoding.decodeStrictByteStringExplicit'
-- * 'Encoding.decodeLazyByteStringExplicit'
-- * 'Encoding.decodeStringExplicit'
--
--
-- == Warnings
--
-- These conversions are provides AS-IS and assume that /encoding/ functions behave
-- in a way consistent with /typed-encoding/ type definitions. 
-- 
-- For example, /typed-encoding/ effectively guarantees that decode function will never fail
-- and it is safe to use @Identity@ instance of the @UnexpectedDecodeErr@ class.
--
-- In /encoding v0.8.5/, the decoding can fail after the encoding succeeded:
--
-- >>> Encoding.encodeStringExplicit EncCP932.CP932 "\DEL"
-- Right "\DEL"
-- >>> Encoding.decodeStringExplicit EncCP932.CP932 "\DEL"
-- Left (IllegalCharacter 127)
--
-- Here are some other peculiarities:
--
-- /cp1257/ is a single bit encoding and one could expect this to fail, but it succeeds:
--
-- >>> Encoding.encodeStringExplicit (Encoding.encodingFromString "cp1257") "\x100"
-- Right "\194"
--
-- Decoding can also be surprising:
--
-- >>> Encoding.decodeStringExplicit EncASCII.ASCII "\236\239"
-- Right "\236\239"
--
-- here is UTF8 decoding
--
-- >>> Encoding.decodeStringExplicit EncUTF8.UTF8 "\192\NUL"
-- Right "\NUL"
-- >>> Encoding.encodeStringExplicit EncUTF8.UTF8 "\NUL"
-- Right "\NUL"
--
-- This package does not try to fix these issues, only provides a wrapper 
-- that annotates encodings as symbols.


module Data.TypedEncoding.Pkg.Encoding.Conv where 

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
-- >>> import           Data.Encoding.ASCII as EncASCII
-- >>> import           Data.Encoding.UTF8 as EncUTF8
-- >>> import           Data.Encoding.CP932 as EncCP932

type family IsDynEnc (s :: Symbol) :: Bool where
    IsDynEnc s = Typed.AcceptEq ('Text "Not encoding restriction " ':<>: ShowType s ) (CmpSymbol (Typed.TakeUntil s ":") "enc-pkg/encoding")

type DynEnc s = (KnownSymbol s, IsDynEnc s ~ 'True)



-- * Conversion To ByteString

-- |
-- @encodeStrictByteStringExplicit@ creates values of types like
--
-- @
-- Enc '["enc-pkg/encoding:cyrillic"] () ByteString
-- @
-- 
-- >>> fmap Typed.displ . encodeStrictByteStringExplicit @"enc-pkg/encoding:cyrillic" . Typed.toEncoding () $ "а на животе кнопка"
-- Right "Enc '[enc-pkg/encoding:cyrillic] () (ByteString \208 \221\208 \214\216\210\222\226\213 \218\221\222\223\218\208)"
-- 
-- >>> fmap Typed.displ . encodeStrictByteStringExplicit @"enc-pkg/encoding:koi8_r" . Typed.toEncoding () $ "а на животе кнопка"
-- Right "Enc '[enc-pkg/encoding:koi8_r] () (ByteString \193 \206\193 \214\201\215\207\212\197 \203\206\207\208\203\193)"
--
-- >>> "а на животе кнопка"
-- "\1072 \1085\1072 \1078\1080\1074\1086\1090\1077 \1082\1085\1086\1087\1082\1072"
--
-- >>> "Статья"
-- "\1057\1090\1072\1090\1100\1103"
--
-- >>> fmap Typed.displ . encodeStrictByteStringExplicit @"enc-pkg/encoding:cyrillic" . Typed.toEncoding () $ "Статья"
-- Right "Enc '[enc-pkg/encoding:cyrillic] () (ByteString \193\226\208\226\236\239)"
--
-- >>> encodeStrictByteStringExplicit @"enc-pkg/encoding:ascii" . Typed.toEncoding () $ "Статья"
-- Left (EncodeEx "enc-pkg/encoding:ascii" (HasNoRepresentation '\1057'))
--
-- >>> fmap Typed.displ . encodeStrictByteStringExplicit @"enc-pkg/encoding:ascii" . Typed.toEncoding () $ "story"
-- Right "Enc '[enc-pkg/encoding:ascii] () (ByteString story)"
encodeStrictByteStringExplicit :: forall s xs c .
             (
                DynEnc s
               , Typed.Algorithm s "enc-pkg/encoding"
               ) => 
              Typed.Enc xs c String -> Either Typed.EncodeEx (Typed.Enc (s ': xs) c B.ByteString)
encodeStrictByteStringExplicit s = 
    do 
        enc <- Typed.asEncodeEx p . exferDynEncoding $ p
        Typed.withUnsafeCoerceF (Typed.asEncodeEx p . Encoding.encodeStrictByteStringExplicit enc) s
        where 
          p = Proxy :: Proxy s


-- | 
-- Converts 'String' to some @Enc '["enc-pkg/encoding:..."] () BL.ByteString@ type
-- by actually encoding characters in the String into correct byte layout.
encodeLazyByteStringExplicit :: forall s xs c  .
             (
                DynEnc s
                , Typed.Algorithm s "enc-pkg/encoding"
                ) => 
              Typed.Enc xs c String -> Either Typed.EncodeEx (Typed.Enc (s ': xs) c BL.ByteString)
encodeLazyByteStringExplicit s = 
    do 
        enc <- Typed.asEncodeEx p $ exferDynEncoding p
        Typed.withUnsafeCoerceF (Typed.asEncodeEx p . Encoding.encodeLazyByteStringExplicit enc) s
        where 
          p = Proxy :: Proxy s

-- * Conversion to encoded String

-- | 
-- Converts 'String' to some @Enc '["enc-pkg/encoding:..."] () String@ type
-- by actually encoding characters in the String into a certain byte layout.
-- 
-- The resulting payload has all Characters representing bytes, that is @< \'\\255\'@
encodeStringExplicit :: forall s xs c .
             (
                DynEnc s
                , Typed.Algorithm s "enc-pkg/encoding"
                ) => 
              Typed.Enc xs c String -> Either Typed.EncodeEx (Typed.Enc (s ': xs) c String)
encodeStringExplicit s = 
    do 
        enc <- Typed.asEncodeEx p $ exferDynEncoding p
        Typed.withUnsafeCoerceF (Typed.asEncodeEx p . Encoding.encodeStringExplicit enc) s
        where 
          p = Proxy :: Proxy s



encString :: forall s xs c .
             (
                DynEnc s
                , Typed.Algorithm s "enc-pkg/encoding"
                ) => 
              Typed.Encoding (Either Typed.EncodeEx) s "enc-pkg/encoding" c String
encString = Typed._mkEncoding encodeStringExplicit



-- * Conversion From ByteString

-- |
--
-- >>> fmap Typed.displ $ decodeStrictByteStringExplicit @"enc-pkg/encoding:cyrillic" @'[] @Identity (Typed.unsafeSetPayload () "\193\226\208\226\236\239")
-- Identity "Enc '[] () (String \1057\1090\1072\1090\1100\1103)"
decodeStrictByteStringExplicit :: forall s xs f c .
                     (Typed.UnexpectedDecodeErr f
                     , Monad f
                     , DynEnc s
                     , Typed.Algorithm s "enc-pkg/encoding"
                    ) => 
                     Typed.Enc (s ': xs) c B.ByteString -> f (Typed.Enc xs c String)
decodeStrictByteStringExplicit x = 
  do 
    enc <- Typed.asUnexpected @s . exferDynEncoding $ p
    Typed.withUnsafeCoerceF (Typed.asUnexpected @s . Encoding.decodeStrictByteStringExplicit enc) x
    where p = Proxy :: Proxy s


decodeLazyByteStringExplicit :: forall s xs f c .
                     (Typed.UnexpectedDecodeErr f
                     , Monad f
                     , DynEnc s
                     , Typed.Algorithm s "enc-pkg/encoding"
                    ) => 
                     Typed.Enc (s ': xs) c BL.ByteString -> f (Typed.Enc xs c String)
decodeLazyByteStringExplicit x = 
  do 
    enc <- Typed.asUnexpected @s . exferDynEncoding $ p
    Typed.withUnsafeCoerceF (Typed.asUnexpected @s . Encoding.decodeLazyByteStringExplicit enc) x
  where p = Proxy :: Proxy s


-- * Conversion From encoded String

decodeStringExplicit :: forall s xs f c .
                     (Typed.UnexpectedDecodeErr f
                     , Monad f
                     , DynEnc s
                     , Typed.Algorithm s "enc-pkg/encoding"
                    ) => 
                     Typed.Enc (s ': xs) c String -> f (Typed.Enc xs c String)
decodeStringExplicit x = 
  do 
    enc <- Typed.asUnexpected @s . exferDynEncoding $ p
    Typed.withUnsafeCoerceF (Typed.asUnexpected @s . Encoding.decodeStringExplicit enc) x
  where p = Proxy :: Proxy s

decString :: forall s xs f c .
                     (Typed.UnexpectedDecodeErr f
                     , Monad f
                     , DynEnc s
                     , Typed.Algorithm s "enc-pkg/encoding"
                    ) => 
                     Typed.Decoding f s "enc-pkg/encoding" c String

decString = Typed.mkDecoding decodeStringExplicit



-- * Helpers
-- | Provides type safety over existence of 'Encoding.DynEncoding'
getDynEncoding :: forall s xs c str. (DynEnc s) => Typed.Enc (s ': xs) c str -> Encoding.DynEncoding
getDynEncoding _ = Encoding.encodingFromString nm 
  where 
      p = Proxy :: Proxy s
      nm = L.drop 8 . symbolVal $ p

-- * Implementation

exferDynEncoding :: (KnownSymbol s, DynEnc s) => Proxy s -> Either String Encoding.DynEncoding
exferDynEncoding p = explainMaybe ("Invalid encoding " ++ nm) . Encoding.encodingFromStringExplicit $ nm 
  where 
      nm = L.drop 17 . symbolVal $ p
      explainMaybe _ (Just x) = Right x
      explainMaybe msg Nothing = Left msg

