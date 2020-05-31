{-# LANGUAGE MultiParamTypeClasses #-}
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
-- Key instances defined here are 'Typed.ToEncString' and 'Typed.FromEncString'.
-- These allow to create encoded @ByteString@ from a String (@ToEncString@) and decode @ByteString@ back (@ToEncString@).
--
-- The String 'Typed.Encode', 'Typed.Decode' instances is less interesting since is works with @String@ type only.
--
-- See the /warnings/ section of 
-- "Data.TypedEncoding.Pkg.Encoding.Conv"


module Data.TypedEncoding.Pkg.Encoding.Instances where 

import qualified Data.TypedEncoding.Instances.Support as Typed

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.TypedEncoding.Pkg.Encoding.Conv as Conv

-- $setup
-- >>> :set -XOverloadedStrings -XDataKinds -XTypeApplications -XFlexibleContexts
-- >>> import           Data.Functor.Identity
-- >>> import qualified Data.TypedEncoding as Usage


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
-- This defines @EncSuperset s@ as @"r-CHAR8"@
instance Conv.DynEnc s => Typed.EncodingSuperset s where
     type EncSuperset s = "r-CHAR8"

