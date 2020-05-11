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

import           Data.TypedEncoding.Internal.Util.TypeLits
import           Data.TypedEncoding.Instances.Support

import           Data.Encoding

import           GHC.TypeLits
import           Data.Proxy
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- $setup
-- >>> :set -XOverloadedStrings -XDataKinds -XTypeApplications
-- >>> import           Data.Functor.Identity

type family IsEnc (s :: Symbol) :: Bool where
    IsEnc s = AcceptEq ('Text "Not encoding restriction " ':<>: ShowType s ) (CmpSymbol "r-enc:" (Take 6 s))

-- | Encoded witness makes this type safe
getDynEncoding :: forall s xs c str. (KnownSymbol s, IsEnc s ~ 'True) => Enc (s ': xs) c str -> DynEncoding
getDynEncoding _ = encodingFromString nm 
  where 
      p = Proxy :: Proxy s
      nm = L.drop 6 . symbolVal $ p


-- | combiniator version of 'EncodeF'
encByteString :: forall f s xs c .
              (
                KnownSymbol s
              , IsEnc s ~ 'True
              , f ~ Either EncodeEx
              ) => 
              Enc xs c B.ByteString -> Either EncodeEx (Enc (s ': xs) c B.ByteString)  
encByteString = implTranF (verifyEncoding (Proxy :: Proxy s) verifyDynEncoding decodeStrictByteStringExplicit)              

-- | Combiniator version of 'EncodeF'
--
-- @"r-enc:"@ restriction encoding of @ByteString@ 
--
-- @"r-" encoding simply verifies that restriction is correct.
encByteStringL :: forall f s xs c .
              (
                KnownSymbol s
              , IsEnc s ~ 'True
              , f ~ Either EncodeEx
              ) => 
              Enc xs c BL.ByteString -> Either EncodeEx (Enc (s ': xs) c BL.ByteString)  
encByteStringL = implTranF (verifyEncoding (Proxy :: Proxy s) verifyDynEncoding decodeLazyByteStringExplicit)              



-- | Combiniator version of 'ToEncString'
-- 
-- >>> toEncByteString @"r-enc:cyrillic" "Статья"
-- Right (MkEnc Proxy () "\193\226\208\226\236\239")
--
-- >>> "Статья"
-- "\1057\1090\1072\1090\1100\1103"
toEncByteString :: forall s .
             (
                KnownSymbol s
              , IsEnc s ~ 'True
                ) => 
              String -> Either EncodeEx (Enc '[s] () B.ByteString)
toEncByteString s = 
    do 
        enc <- cvrtToEncodeEx p . verifyDynEncoding $ p
        fmap (unsafeSetPayload ()) . cvrtToEncodeEx p . encodeStrictByteStringExplicit enc $ s
        where 
          p = Proxy :: Proxy s

-- | Combiniator version of 'ToEncString'
--
-- It converts 'String' to some @Enc '["r-enc:..."] () BL.ByteString@ type
-- by actually encoding characters in the String into correct byte layout.
toEncByteStringL :: forall s .
             (
                KnownSymbol s
              , IsEnc s ~ 'True
                ) => 
              String -> Either EncodeEx (Enc '[s] () BL.ByteString)
toEncByteStringL s = 
    do 
        enc <- cvrtToEncodeEx p $ verifyDynEncoding p
        fmap (unsafeSetPayload ()) . cvrtToEncodeEx p . encodeLazyByteStringExplicit enc $ s
        where 
          p = Proxy :: Proxy s


-- | Combiniator version of 'FromEncString'
--
-- >>> fromEncByteString @"r-enc:cyrillic" @Identity (unsafeSetPayload () "\193\226\208\226\236\239")
-- Identity "\1057\1090\1072\1090\1100\1103"
fromEncByteString :: forall s f .
                     (UnexpectedDecodeErr f
                     , Monad f
                     , KnownSymbol s
                     , IsEnc s ~ 'True
                     ) => 
                     Enc '[s] () B.ByteString -> f String
fromEncByteString x = 
  do 
    enc <- asUnexpected @s . verifyDynEncoding $ p
    asUnexpected @s . decodeStrictByteStringExplicit enc . getPayload $ x
  where p = Proxy :: Proxy s


-- | Combiniator version of 'FromEncString'
fromEncByteStringL :: forall s f .
                     (UnexpectedDecodeErr f
                     , Monad f
                     , KnownSymbol s
                     , IsEnc s ~ 'True
                     ) => 
                     Enc '[s] () BL.ByteString -> f String
fromEncByteStringL x = 
  do 
    enc <- asUnexpected @s . verifyDynEncoding $ p
    asUnexpected @s . decodeLazyByteStringExplicit enc . getPayload $ x
  where p = Proxy :: Proxy s

-- private ---

verifyDynEncoding :: (KnownSymbol s, IsEnc s ~ 'True) => Proxy s -> Either String DynEncoding
verifyDynEncoding p = explainMaybe ("Invalid encoding " ++ nm) . encodingFromStringExplicit $ nm 
  where 
      nm = L.drop 6 . symbolVal $ p
      explainMaybe _ (Just x) = Right x
      explainMaybe msg Nothing = Left msg


-- could be moved to main project
verifyEncoding :: forall s str err1 err2 enc a. (KnownSymbol s, Show err1, Show err2) => Proxy s -> (Proxy s -> Either err1 enc) -> (enc -> str -> Either err2 a) -> str -> Either EncodeEx str
verifyEncoding p findenc decoder str = 
  do
    enc <- cvrtToEncodeEx p . findenc $ p
    case decoder enc str of
      Left err -> Left $ EncodeEx p err
      Right r -> Right str

cvrtToEncodeEx
  :: (Show a, KnownSymbol x) => Proxy x -> Either a b -> Either EncodeEx b
cvrtToEncodeEx p = either (Left . EncodeEx p) Right 

