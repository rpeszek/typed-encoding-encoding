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
-- There seems to be no easy ways to verify encoding using the /encoding/ package. 
--
-- /decode/ functions implemented in /encoding/ are very forgiving and work 
-- on invalid encoded inputs. This forces this package to resort to checking that 
--
-- @
-- Encoding.encodeXyz . Encoding.decodeXyz
-- @
-- 
-- acts as the identity.  This is obviously quite expensive. 
-- 
-- This module provides such implementation and hence the warning.
--
-- >>> Encoding.decodeStrictByteStringExplicit EncUTF8.UTF8 "\192\NUL"
-- Right "\NUL"
--
-- >>> Encoding.encodeStrictByteStringExplicit EncUTF8.UTF8 "\NUL"
-- Right "\NUL"


module Data.TypedEncoding.Pkg.Encoding.Warn.Instances {-# WARNING "Not optimized for performance" #-} where 

import qualified Data.TypedEncoding.Instances.Support as Typed
import qualified Data.Encoding as Encoding

import           GHC.TypeLits
import           Data.Proxy

import           Data.TypedEncoding.Pkg.Encoding.Conv


-- import qualified Data.TypedEncoding as Usage
-- import           Data.Encoding.UTF8 as EncUTF8


-- $setup
-- >>> :set -XOverloadedStrings -XDataKinds -XTypeApplications -XFlexibleContexts
-- >>> import           Data.Functor.Identity
-- >>> import qualified Data.TypedEncoding as Usage
-- >>> import           Data.Encoding.UTF8 as EncUTF8


-- * Validation Combinators (Slow)

data DecodeOrEncodeException = 
  DecErr Encoding.DecodingException 
  | EncErr Encoding.EncodingException 
  | DecEncMismatch 
  deriving (Show, Eq)


validatingDecS :: forall s c .
              (
                DynEnc s
                , Typed.Algorithm s "enc-pkg/encoding"
               ) => 
               Typed.Decoding (Either Typed.UnexpectedDecodeEx) s "enc-pkg/encoding" c String
validatingDecS = Typed._implDecodingF (verifyDynDec (Proxy :: Proxy s) exferDynEncoding slowValidation)   
   where 
     -- see comments in fromDynEncB
     slowValidation enc str = do
        dec <- either (Left . DecErr) Right . Encoding.decodeStringExplicit enc $ str
        res <- either (Left . EncErr) Right . Encoding.encodeStringExplicit enc $ dec  
        if str == res 
        then Right dec
        else Left DecEncMismatch      


-- tst = fmap Usage.displ .
--   Usage.recreateFAll' 
--    @'["enc-pkg/encoding"] 
--    @'["enc-pkg/encoding:cyrillic"] 
--    @(Either Usage.RecreateEx) 
--    @() 
--    @String . Usage.toEncoding () $ "\193\226\208\226\236\239"


-- |
--
-- >>> :{ 
--  fmap Usage.displ .
--   Usage.recreateFAll' 
--    @'["enc-pkg/encoding"] 
--    @'["enc-pkg/encoding:greek"] 
--    @(Either Usage.RecreateEx) 
--    @() 
--    @String . Usage.toEncoding () $ "\193\226\208\226\236\255"
-- :}
-- Left (RecreateEx "enc-pkg/encoding:greek" (DecErr (IllegalCharacter 255)))
--
--  @\"Статья\"@ example:
--
-- >>> :{ 
-- fmap Usage.displ .
--   Usage.recreateFAll' 
--   @'["enc-pkg/encoding"] 
--   @'["enc-pkg/encoding:cyrillic"] 
--   @(Either Usage.RecreateEx) 
--   @() 
--   @String . Usage.toEncoding () $ "\193\226\208\226\236\239"
-- :}
-- Right "Enc '[enc-pkg/encoding:cyrillic] () (String \193\226\208\226\236\239)"
instance (KnownSymbol s , DynEnc s, Typed.Algorithm s "enc-pkg/encoding", Typed.RecreateErr f, Applicative f) => Typed.Validate f s "enc-pkg/encoding" c String where
    validation = Typed.validFromDec' @"enc-pkg/encoding" validatingDecS



verifyDynDec :: forall s str err1 err2 enc a. (KnownSymbol s, Show err1, Show err2) => 
                  Proxy s   -- ^ proxy defining encoding annotation
                  -> (Proxy s -> Either err1 enc)  -- ^ finds encoding marker @enc@ for given annotation or fails
                  -> (enc -> str -> Either err2 str)  -- ^ decoder based on @enc@ marker
                  -> str
                  -> Either Typed.UnexpectedDecodeEx str
verifyDynDec p findenc decoder str = 
  do
    enc <- either (Left . Typed.UnexpectedDecodeEx p) Right . findenc $ p
    case decoder enc str of
      Left err -> Left $ Typed.UnexpectedDecodeEx p err
      Right r -> Right r
