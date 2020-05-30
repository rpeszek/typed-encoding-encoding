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
-- Key instances defined here are 'Typed.ToEncString' and 'Typed.FromEncString'.
-- These allow to create encoded @ByteString@ from a String (@ToEncString@) and decode @ByteString@ back (@ToEncString@).
--
-- This module also implements Encode / Decode instances for @String@,
--
-- Validate instance is 
--  
-- There seems to be no easy way to verify encoding using the /encoding/ package. 
-- Provided decode functions are very forgiving and work 
-- on invalid encoded inputs. This forces us to resort to checking that 
--
-- @
-- Encoding.encodeXyz . Encoding.decodeXyz
-- @
-- 
-- acts are identity.  This is obviously expensive. 
-- 
-- This module provides such implementation with a warning.
--
-- -- | Encoding ByteString just verifies its byte layout
-- -- 
-- -- /encoding/ package decoding is very forgiving, can decode invalid data, this makes things hard:
-- --
-- -- Encoding.decodeStrictByteStringExplicit EncUTF8.UTF8 "\192\NUL"
-- -- Right "\NUL"
-- -- Encoding.encodeStrictByteStringExplicit EncUTF8.UTF8 "\NUL"
-- -- "\NUL"


module Data.TypedEncoding.Pkg.Encoding.Warn.Instances {-# WARNING "Not optimized for performance" #-} where 



import qualified Data.TypedEncoding.Instances.Support as Typed
import qualified Data.Encoding as Encoding

import           GHC.TypeLits
import           Data.Proxy
-- import qualified Data.List as L
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy as BL

-- import           Data.Encoding.UTF8 as EncUTF8

import           Data.TypedEncoding.Pkg.Encoding.Conv



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


-- -- |
-- -- For "r-" encodings this is the same as @Encode@.
-- --
-- -- >>> :{ 
-- -- fmap Usage.displ 
-- --   . Usage.recreateFAll' 
-- --   @'["enc-pkg/encoding"] 
-- --   @'["enc-pkg/encoding:greek"] 
-- --   @(Either Usage.RecreateEx) 
-- --   @() 
-- --   @B.ByteString  
-- --   . Usage.toEncoding () $ "\193\226\208\226\236\255"
-- -- :}
-- -- Left (RecreateEx "enc-pkg/encoding:greek" (DecErr (IllegalCharacter 255)))
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
