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

-- | This package is a thin layer over the /encoding/ package to create types compatible
-- with /type-encoding/.
--
-- This module contains combinators which are polymorphic in the symbol annotation.
--
-- Sadly, /encoding/ library does not provide ways to encode and decode the popular @Text@ type (from the /text/ package). 
-- Also there seems to be no easy way to verify encoding. Provided decode functions are very forgiving and work 
-- on invalid encoded inputs. This forces us to resort to checking that @Encoding.encodeXyz . Encoding.decodeXyz@ 
-- acts are identity.  This is obviously expensive.  
--
-- Key instances defined here are 'Typed.ToEncString' and 'Typed.FromEncString'.
-- These allow to create encoded @ByteString@ from a String (@ToEncString@) and decode @ByteString@ back (@ToEncString@).
--
-- Other instances are less interesting, are very expensive, and only provide validation facilities.
--
-- It should be possible to create more efficient versions of 'Typed.Encode' in the future, which do not use String decoding under the hood
-- but because of the forgiving nature of decode architecture in this library this may not fix the core performance problem.
--
-- == Naming conventions
-- 
-- @"r-pkg/encoding:[encoding]"@ - where @"[encoding]"@ is String name used by the 'Encoding.DynEncoding' in the /encoding/ package.
--
-- Example: @"r-pkg/encoding:cyrillic"@

module Data.TypedEncoding.Pkg.Instances.Restriction.Encoding where 

import qualified Data.TypedEncoding.Instances.Support as Typed

import qualified Data.Encoding as Encoding

import           GHC.TypeLits
import           Data.Proxy
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import           Data.Encoding.UTF8 as EncUTF8

-- $setup
-- >>> :set -XOverloadedStrings -XDataKinds -XTypeApplications -XFlexibleContexts
-- >>> import           Data.Functor.Identity
-- >>> import qualified Data.TypedEncoding as Usage
-- >>> import           Data.Encoding.ASCII as EncASCII
-- >>> import           Data.Encoding.UTF8 as EncUTF8

type family IsDynEnc (s :: Symbol) :: Bool where
    IsDynEnc s = Typed.AcceptEq ('Text "Not encoding restriction " ':<>: ShowType s ) (CmpSymbol (Typed.TakeUntil s ":") "r-pkg/encoding")

type DynEnc s = (KnownSymbol s, IsDynEnc s ~ 'True)

-- |
-- 
-- >>> :{
-- Usage.displ 
--  <$> Usage.toEncStringF' 
--   @"r-pkg/encoding" 
--   @"r-pkg/encoding:cyrillic" 
--   @(Either Usage.EncodeEx) 
--   @String 
--   @B.ByteString "Статья"
-- :}
-- Right "Enc '[r-pkg/encoding:cyrillic] () (ByteString \193\226\208\226\236\239)"

instance (DynEnc s, Typed.Algorithm s "r-pkg/encoding") => Typed.ToEncString (Either Typed.EncodeEx) s "r-pkg/encoding" String B.ByteString where
  toEncF = toDynEncB

instance (DynEnc s, Typed.Algorithm s "r-pkg/encoding") => Typed.ToEncString (Either Typed.EncodeEx) s "r-pkg/encoding" String BL.ByteString where
  toEncF = toDynEncBL

instance (DynEnc s, Typed.Algorithm s "r-pkg/encoding") => Typed.ToEncString (Either Typed.EncodeEx) s "r-pkg/encoding" String String where
  toEncF = toDynEncS

-- |
-- 
-- >>> :{
-- Usage.displ <$> 
--  Usage.fromEncStringF' 
--   @"r-pkg/encoding" 
--   @"r-pkg/encoding:cyrillic" 
--   @(Either Usage.UnexpectedDecodeEx) 
--   @String @B.ByteString 
--    (Typed.unsafeSetPayload () "\193\226\208\226\236\239")
-- :}
-- Right "(String \1057\1090\1072\1090\1100\1103)"
instance (Typed.UnexpectedDecodeErr f, Monad f, DynEnc s, Typed.Algorithm s "r-pkg/encoding") => Typed.FromEncString f s "r-pkg/encoding" String B.ByteString where
  fromEncF = fromDynEncB

instance (Typed.UnexpectedDecodeErr f, Monad f, DynEnc s, Typed.Algorithm s "r-pkg/encoding") => Typed.FromEncString f s "r-pkg/encoding" String BL.ByteString where
  fromEncF = fromDynEncBL

instance (Typed.UnexpectedDecodeErr f, Monad f, DynEnc s, Typed.Algorithm s "r-pkg/encoding") => Typed.FromEncString f s "r-pkg/encoding" String String where
  fromEncF = fromDynEncS


-------------------------
-- Questionable Encode / Validate / Decode instances
-------------------------

-- |
-- >>> :{ 
-- fmap Usage.displ 
--  . Usage.encodeFAll' 
--     @'["r-pkg/encoding"] 
--     @'["r-pkg/encoding:cyrillic"] 
--     @(Either Usage.EncodeEx) 
--     @() 
--     @B.ByteString 
--     . Usage.toEncoding () $ "\193\226\208\226\236\255"
-- :}
-- Right "Enc '[r-pkg/encoding:cyrillic] () (ByteString \193\226\208\226\236\255)"
--
-- >>> :{ 
-- fmap Usage.displ 
--  . Usage.encodeFAll' 
--   @'["r-pkg/encoding"] 
--   @'["r-pkg/encoding:greek"] 
--   @(Either Usage.EncodeEx) 
--   @() 
--   @B.ByteString 
--   . Usage.toEncoding () $ "\193\226\208\226\236\255"
-- :}
-- Left (EncodeEx "r-pkg/encoding:greek" (DecErr (IllegalCharacter 255)))
instance (DynEnc s, Typed.Algorithm s "r-pkg/encoding") => Typed.Encode (Either Typed.EncodeEx) s "r-pkg/encoding" c B.ByteString where
    encoding = encDynB

instance (DynEnc s, Typed.Algorithm s "r-pkg/encoding") => Typed.Encode (Either Typed.EncodeEx) s "r-pkg/encoding" c BL.ByteString where
    encoding = encDynBL

instance (KnownSymbol s, Typed.Restriction s, Typed.Algorithm s "r-pkg/encoding", Applicative f) => Typed.Decode f s "r-pkg/encoding" c str where
    decoding = Typed.decAnyR_

-- |
-- For "r-" encodings this is the same as @Encode@.
--
-- >>> :{ 
-- fmap Usage.displ 
--   . Usage.recreateFAll' 
--   @'["r-pkg/encoding"] 
--   @'["r-pkg/encoding:greek"] 
--   @(Either Usage.RecreateEx) 
--   @() 
--   @B.ByteString  
--   . Usage.toEncoding () $ "\193\226\208\226\236\255"
-- :}
-- Left (RecreateEx "r-pkg/encoding:greek" (DecErr (IllegalCharacter 255)))
instance (KnownSymbol s , DynEnc s, Typed.Algorithm s "r-pkg/encoding", Typed.RecreateErr f, Applicative f) => Typed.Validate f s "r-pkg/encoding" c B.ByteString where
    validation = Typed.validFromEnc' @"r-pkg/encoding" encDynB

-------------------------
-- End Questionable Encode / Validate / Decode instances
-------------------------

-- * Conversion To ByteString

-- |
-- 
-- >>> Typed.displ <$> toDynEncB @"r-pkg/encoding:cyrillic" "а на животе кнопка"
-- Right "Enc '[r-pkg/encoding:cyrillic] () (ByteString \208 \221\208 \214\216\210\222\226\213 \218\221\222\223\218\208)"
-- 
-- >>> Typed.displ <$> toDynEncB @"r-pkg/encoding:koi8_r" "а на животе кнопка"
-- Right "Enc '[r-pkg/encoding:koi8_r] () (ByteString \193 \206\193 \214\201\215\207\212\197 \203\206\207\208\203\193)"
--
-- >>> "а на животе кнопка"
-- "\1072 \1085\1072 \1078\1080\1074\1086\1090\1077 \1082\1085\1086\1087\1082\1072"
--
-- >>> "Статья"
-- "\1057\1090\1072\1090\1100\1103"
--
-- >>> toDynEncB @"r-pkg/encoding:ascii" "Статья"
-- Left (EncodeEx "r-pkg/encoding:ascii" (HasNoRepresentation '\1057'))
toDynEncB :: forall s .
             (
                DynEnc s
               , Typed.Algorithm s "r-pkg/encoding"
               ) => 
              String -> Either Typed.EncodeEx (Typed.Enc '[s] () B.ByteString)
toDynEncB s = 
    do 
        enc <- Typed.asEncodeEx p . exferDynEncoding $ p
        fmap (Typed.unsafeSetPayload ()) . Typed.asEncodeEx p . Encoding.encodeStrictByteStringExplicit enc $ s
        where 
          p = Proxy :: Proxy s


-- | 
-- Converts 'String' to some @Enc '["r-pkg/encoding:..."] () BL.ByteString@ type
-- by actually encoding characters in the String into correct byte layout.
toDynEncBL :: forall s .
             (
                DynEnc s
                , Typed.Algorithm s "r-pkg/encoding"
                ) => 
              String -> Either Typed.EncodeEx (Typed.Enc '[s] () BL.ByteString)
toDynEncBL s = 
    do 
        enc <- Typed.asEncodeEx p $ exferDynEncoding p
        fmap (Typed.unsafeSetPayload ()) . Typed.asEncodeEx p . Encoding.encodeLazyByteStringExplicit enc $ s
        where 
          p = Proxy :: Proxy s

-- * Conversion to encoded String

-- | 
-- Converts 'String' to some @Enc '["r-pkg/encoding:..."] () String@ type
-- by actually encoding characters in the String into correct byte layout.
toDynEncS :: forall s .
             (
                DynEnc s
                , Typed.Algorithm s "r-pkg/encoding"
                ) => 
              String -> Either Typed.EncodeEx (Typed.Enc '[s] () String)
toDynEncS s = 
    do 
        enc <- Typed.asEncodeEx p $ exferDynEncoding p
        fmap (Typed.unsafeSetPayload ()) . Typed.asEncodeEx p . Encoding.encodeStringExplicit enc $ s
        where 
          p = Proxy :: Proxy s


-- * Conversion From ByteString

-- |
--
-- >>> fromDynEncB @"r-pkg/encoding:cyrillic" @Identity (Typed.unsafeSetPayload () "\193\226\208\226\236\239")
-- Identity "\1057\1090\1072\1090\1100\1103"
--
-- The following example demonstrates an interesting bit about the /encoding/ package, we are decoding not an /ASCII/ encoding:
--
-- >>> fromDynEncB @"r-pkg/encoding:ascii" @Identity (Typed.unsafeSetPayload () "\193\226\208\226\236\239")
-- Identity "\193\226\208\226\236\239"
--
-- >>> Encoding.decodeStrictByteStringExplicit EncASCII.ASCII "\193\226\208\226\236\239"
-- Right "\193\226\208\226\236\239"
--
-- This is OK, with extra /type-encoding/ type safety the only way to get invalid payload into @"r-pkg/encoding:ascii"@
-- is by using one of the unsafe functions.  One can imagine this being error prone otherwise. 
--
-- Moreover, this prevents easy creation of 'Typed.Encode' instances!
fromDynEncB :: forall s f .
                     (Typed.UnexpectedDecodeErr f
                     , Monad f
                     , DynEnc s
                     , Typed.Algorithm s "r-pkg/encoding"
                    ) => 
                     Typed.Enc '[s] () B.ByteString -> f String
fromDynEncB x = 
  do 
    enc <- Typed.asUnexpected @s . exferDynEncoding $ p
    Typed.asUnexpected @s . Encoding.decodeStrictByteStringExplicit enc . Typed.getPayload $ x
  where p = Proxy :: Proxy s


fromDynEncBL :: forall s f .
                     (Typed.UnexpectedDecodeErr f
                     , Monad f
                     , DynEnc s
                     , Typed.Algorithm s "r-pkg/encoding"
                    ) => 
                     Typed.Enc '[s] () BL.ByteString -> f String
fromDynEncBL x = 
  do 
    enc <- Typed.asUnexpected @s . exferDynEncoding $ p
    Typed.asUnexpected @s . Encoding.decodeLazyByteStringExplicit enc . Typed.getPayload $ x
  where p = Proxy :: Proxy s


-- * Conversion From encoded String

fromDynEncS :: forall s f .
                     (Typed.UnexpectedDecodeErr f
                     , Monad f
                     , DynEnc s
                     , Typed.Algorithm s "r-pkg/encoding"
                    ) => 
                     Typed.Enc '[s] () String -> f String
fromDynEncS x = 
  do 
    enc <- Typed.asUnexpected @s . exferDynEncoding $ p
    Typed.asUnexpected @s . Encoding.decodeStringExplicit enc . Typed.getPayload $ x
  where p = Proxy :: Proxy s



-- * Encoding Combinators (Slow)

data DecodeOrEncodeException = 
  DecErr Encoding.DecodingException 
  | EncErr Encoding.EncodingException 
  | DecEncMismatch 
  deriving (Show, Eq)


-- | Encoding ByteString just verifies its byte layout
-- 
-- /encoding/ package decoding is very forgiving, can decode invalid data, this makes things hard:
--
-- Encoding.decodeStrictByteStringExplicit EncUTF8.UTF8 "\192\NUL"
-- Right "\NUL"
-- Encoding.encodeStrictByteStringExplicit EncUTF8.UTF8 "\NUL"
-- "\NUL"
--
-- >>> Encoding.decodeStrictByteStringExplicit EncASCII.ASCII "\239"
-- Right "\239"
encDynB :: forall s c .
              (
                DynEnc s
                , Typed.Algorithm s "r-pkg/encoding"
               ) => 
               Typed.Encoding (Either Typed.EncodeEx) s "r-pkg/encoding" c B.ByteString
encDynB = Typed._implEncodingEncodeEx @s  (Typed.verifyDynEnc (Proxy :: Proxy s) exferDynEncoding slowValidation)   
   where 
     -- see comments in fromDynEncB
     slowValidation enc str = do
        dec <- either (Left . DecErr) Right . Encoding.decodeStrictByteStringExplicit enc $ str
        res <- either (Left . EncErr) Right . Encoding.encodeStrictByteStringExplicit enc $ dec  
        if str == res 
        then Right str
        else Left DecEncMismatch      

encDynBL :: forall s xs c .
              (
               DynEnc s
               , Typed.Algorithm s "r-pkg/encoding"
              ) => 
              Typed.Encoding (Either Typed.EncodeEx) s "r-pkg/encoding" c BL.ByteString
encDynBL = Typed._implEncodingEncodeEx @s (Typed.verifyDynEnc (Proxy :: Proxy s) exferDynEncoding slowValidation)              
   where 
     -- see comments in fromDynEncB
     slowValidation enc str = do
        dec <- either (Left . DecErr) Right . Encoding.decodeLazyByteStringExplicit enc $ str
        res <- either (Left . EncErr) Right . Encoding.encodeLazyByteStringExplicit enc $ dec         
        if str == res 
        then Right str
        else Left DecEncMismatch      

encDynS :: forall s c .
              (
                DynEnc s
                , Typed.Algorithm s "r-pkg/encoding"
               ) => 
               Typed.Encoding (Either Typed.EncodeEx) s "r-pkg/encoding" c String
encDynS = Typed._implEncodingEncodeEx @s  (Typed.verifyDynEnc (Proxy :: Proxy s) exferDynEncoding slowValidation)   
   where 
     -- see comments in fromDynEncB
     slowValidation enc str = do
        dec <- either (Left . DecErr) Right . Encoding.decodeStringExplicit enc $ str
        res <- either (Left . EncErr) Right . Encoding.encodeStringExplicit enc $ dec  
        if str == res 
        then Right str
        else Left DecEncMismatch      


-- * Helpers
-- | Provides type safety over existence of 'Encoding.DynEncoding'
getDynEncoding :: forall s xs c str. (DynEnc s) => Typed.Enc (s ': xs) c str -> Encoding.DynEncoding
getDynEncoding _ = Encoding.encodingFromString nm 
  where 
      p = Proxy :: Proxy s
      nm = L.drop 6 . symbolVal $ p

-- * Implementation

exferDynEncoding :: (KnownSymbol s, DynEnc s) => Proxy s -> Either String Encoding.DynEncoding
exferDynEncoding p = explainMaybe ("Invalid encoding " ++ nm) . Encoding.encodingFromStringExplicit $ nm 
  where 
      nm = L.drop 15 . symbolVal $ p
      explainMaybe _ (Just x) = Right x
      explainMaybe msg Nothing = Left msg

-- * (incomplete) mapping set for String / ByteString conversions to work

type instance Typed.IsSupersetOpen "r-ASCII" "r-pkg/encoding:ascii"           x xs = 'True
type instance Typed.IsSupersetOpen "r-ASCII" "r-pkg/encoding:646"             x xs = 'True
type instance Typed.IsSupersetOpen "r-ASCII" "r-pkg/encoding:ansi_x3_4_1968"  x xs = 'True
type instance Typed.IsSupersetOpen "r-ASCII" "r-pkg/encoding:ansi_x3.4_1986"  x xs = 'True
type instance Typed.IsSupersetOpen "r-ASCII" "r-pkg/encoding:cp367"           x xs = 'True
type instance Typed.IsSupersetOpen "r-ASCII" "r-pkg/encoding:csascii"         x xs = 'True
type instance Typed.IsSupersetOpen "r-ASCII" "r-pkg/encoding:ibm367"          x xs = 'True
type instance Typed.IsSupersetOpen "r-ASCII" "r-pkg/encoding:iso646_us"       x xs = 'True
type instance Typed.IsSupersetOpen "r-ASCII" "r-pkg/encoding:iso_646.irv_1991" x xs = 'True
type instance Typed.IsSupersetOpen "r-ASCII" "r-pkg/encoding:iso_ir_6"        x xs = 'True
type instance Typed.IsSupersetOpen "r-ASCII" "r-pkg/encoding:us"              x xs = 'True
type instance Typed.IsSupersetOpen "r-ASCII" "r-pkg/encoding:us_ascii"        x xs = 'True

-- "r-UTF8" has additional Unicode D76 restriction and hence is not superset 
-- type instance Typed.IsSupersetOpen "r-UTF8" "r-pkg/encoding:utf_8"             x xs = 'True   
-- type instance Typed.IsSupersetOpen "r-UTF8" "r-pkg/encoding:utf8"              x xs = 'True  

type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:koi8_r"            x xs = 'True   
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cskoi8r"           x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:koi8_u"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_1"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso8859_1"         x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:8859"              x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp819"             x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:csisolatin1"       x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:ibm819"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso8859"           x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_1_1987"   x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_ir_100"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:l1"                x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:latin"             x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:latin1"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_2"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso8859_2"         x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:csisolatin2"       x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_2_1987"   x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_ir_101"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:l2"                x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:latin2"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_3"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso8859_3"         x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:csisolatin3"       x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_3_1988"   x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_ir_109"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:l3"                x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:latin3"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_4"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso8859_4"         x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:csisolatin4"       x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_4_1988"   x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_ir_110"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:l4"                x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:latin4"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_5"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso8859_5"         x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:csisolatincyrillic"  x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cyrillic"          x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_5_1988"   x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_ir_144"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_6"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso8859_6"         x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:arabic"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:asmo_708"          x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:csisolatinarabic"  x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:ecma_114"          x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_6_1987"   x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_ir_127"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_7"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso8859_7"         x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:csisolatingreek"   x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:ecma_118"          x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:elot_928"          x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:greek"             x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:greek8"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_7_1987"   x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_ir_126"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_8"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso8859_8"         x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:csisolatinhebrew"  x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:hebrew"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_8_1988"   x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_ir_138"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_9"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso8859_9"         x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:csisolatin5"       x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_9_1989"   x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_ir_148"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:l5"                x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:latin5"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_10"       x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso8859_10"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:csisolatin6"       x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_10_1992"  x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_ir_157"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:l6"                x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:latin6"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_11"       x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso8859_11"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:thai"              x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_11_2001"  x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_13"       x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso8859_13"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_14"       x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso8859_14"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_14_1998"  x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_celtic"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_ir_199"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:l8"                x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:latin8"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_15"       x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso8859_15"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:latin9"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:l9"                x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_16"       x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso8859_16"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_8859_16_2001"  x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:iso_ir_226"        x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:l10"               x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:latin10"           x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp1250"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:windows_1250"      x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp1251"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:windows_1251"      x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp1252"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:windows_1252"      x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp1253"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:windows_1253"      x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp1254"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:windows_1254"      x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp1255"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:windows_1255"      x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp1256"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:windows_1256"      x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp1257"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:windows_1257"      x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp1258"            x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:windows_1258"      x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp437"             x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp737"             x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp775"             x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp850"             x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp852"             x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp855"             x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp857"             x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp860"             x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp861"             x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp862"             x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp863"             x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp864"             x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp865"             x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp866"             x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp869"             x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp874"             x xs = 'True  
type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:cp932"             x xs = 'True  

type instance Typed.IsSupersetOpen "r-CHAR8" "r-pkg/encoding:jis_x_0208"        x xs = 'True 
-- :t Tst.pack :: Typed.Enc '["r-pkg/encoding:windows_1255"] c String -> _
