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
-- @"enc-pkg/encoding:[encoding]"@ - where @"[encoding]"@ is String name used by the 'Encoding.DynEncoding' in the /encoding/ package.
--
-- Example: @"enc-pkg/encoding:cyrillic"@
--
-- Superset instances are provided in separate packages located in
--
-- "Data.TypedEncoding.Pkg.Instances.Restriction.Encoding.Superset" 
--
-- with one exception:
--
-- "Data.TypedEncoding.Pkg.Instances.Restriction.Encoding.Warn.UTF8"


module Data.TypedEncoding.Pkg.Encoding.Instances where 

import qualified Data.TypedEncoding.Instances.Support as Typed

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.TypedEncoding.Pkg.Encoding.Conv as Conv

-- $setup
-- >>> :set -XOverloadedStrings -XDataKinds -XTypeApplications -XFlexibleContexts
-- >>> import           Data.Functor.Identity
-- >>> import qualified Data.TypedEncoding as Usage
-- >>> import           Data.Encoding.ASCII as EncASCII
-- >>> import           Data.Encoding.UTF8 as EncUTF8


-- |
-- 
-- >>> :{
-- Usage.displ 
--  <$> Usage.toEncStringF' 
--   @"enc-pkg/encoding" 
--   @"enc-pkg/encoding:cyrillic" 
--   @(Either Usage.EncodeEx) 
--   @String 
--   @B.ByteString "Статья"
-- :}
-- Right "Enc '[enc-pkg/encoding:cyrillic] () (ByteString \193\226\208\226\236\239)"
instance (Conv.DynEnc s, Typed.Algorithm s "enc-pkg/encoding") => Typed.ToEncString (Either Typed.EncodeEx) s "enc-pkg/encoding" String B.ByteString where
  toEncF = Conv.encodeStrictByteStringExplicit . Typed.toEncoding () 

instance (Conv.DynEnc s, Typed.Algorithm s "enc-pkg/encoding") => Typed.ToEncString (Either Typed.EncodeEx) s "enc-pkg/encoding" String BL.ByteString where
  toEncF = Conv.encodeLazyByteStringExplicit . Typed.toEncoding () 

-- |
-- 
-- >>> :{
-- Usage.displ <$> 
--  Usage.fromEncStringF' 
--   @"enc-pkg/encoding" 
--   @"enc-pkg/encoding:cyrillic" 
--   @(Either Usage.UnexpectedDecodeEx) 
--   @String @B.ByteString 
--    (Typed.unsafeSetPayload () "\193\226\208\226\236\239")
-- :}
-- Right "(String \1057\1090\1072\1090\1100\1103)"
instance (
    Typed.UnexpectedDecodeErr f
    , Monad f
    , Conv.DynEnc s
    , Typed.Algorithm s "enc-pkg/encoding"
    ) => Typed.FromEncString f s "enc-pkg/encoding" String B.ByteString where
  fromEncF = fmap Typed.fromEncoding . Conv.decodeStrictByteStringExplicit

instance (
    Typed.UnexpectedDecodeErr f
    , Monad f
    , Conv.DynEnc s
    , Typed.Algorithm s "enc-pkg/encoding"
    ) => Typed.FromEncString f s "enc-pkg/encoding" String BL.ByteString where
  fromEncF = fmap Typed.fromEncoding . Conv.decodeLazyByteStringExplicit



instance (
    Conv.DynEnc s
    , Typed.Algorithm s "enc-pkg/encoding"
    ) => Typed.Encode (Either Typed.EncodeEx) s "enc-pkg/encoding" c String where
    encoding = Conv.encString


instance (
    Typed.UnexpectedDecodeErr f
    , Monad f
    , Conv.DynEnc s
    , Typed.Algorithm s "enc-pkg/encoding"
    ) => Typed.Decode f s "enc-pkg/encoding" c String where
    decoding = Conv.decString

-- | All /encoding/ encodings map characters to bytes
instance Conv.DynEnc s => Typed.EncodingSuperset s where
     type EncSuperset s = "r-CHAR8"

