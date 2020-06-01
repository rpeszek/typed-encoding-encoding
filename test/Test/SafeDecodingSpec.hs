{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

-- | 
module Test.SafeDecodingSpec where


import           Data.TypedEncoding 
import           Data.TypedEncoding.Pkg.Encoding.Conv 
import           Data.TypedEncoding.Pkg.Encoding.Instances ()
import           Data.TypedEncoding.Pkg.Encoding.Warn.Instances ()

import           Data.TypedEncoding 

import           Test.QuickCheck.Instances.ByteString ()
import           Test.QuickCheck
import           Test.QuickCheck.Property
import           Test.Hspec
import           GHC.TypeLits
import           Data.Either

-- | decoding should never fail if encoding succeeds
propEncDec :: forall s . (Algorithm s "enc-pkg/encoding") => 
               Encoding (Either EncodeEx) s "enc-pkg/encoding" () String 
               -> Decoding (Either UnexpectedDecodeEx) s "enc-pkg/encoding" () String 
               -> String 
               -> Property
propEncDec encf decf str = property $ liftBool (_propSafeDecoding @s encf decf () str)
 

propValidation  :: forall s . (Algorithm s "enc-pkg/encoding") => 
                   Encoding (Either EncodeEx) s "enc-pkg/encoding" () String 
                   -> Validation (Either RecreateEx) s "enc-pkg/encoding" () String 
                   -> Decoding (Either UnexpectedDecodeEx) s "enc-pkg/encoding" () String 
                   -> String 
                   -> Property
propValidation encf vf decf str = property $ liftBool (_propSafeValidatedDecoding @s vf decf () modStr)
    -- bit of ugly hack, it would be much more work to control Arbitrary outside of this call
    where 
        modStr = case str of 
            'a': _ -> str                               -- most likely invalid encoding
            _ -> either (const str) getPayload . _runEncoding @s encf . toEncoding () $ str -- most likely a valid encoded string
 

data TestSuite = OneBitOnly | All


suite :: 
       TestSuite
       -> (forall s . (Algorithm s "enc-pkg/encoding", KnownSymbol s) => 
          Decoding (Either UnexpectedDecodeEx) s "enc-pkg/encoding" () String 
          -> String 
          -> Property) 
       -> SpecWith ()
suite _ prop = do 
    it "utf8"          $ property $ prop @"enc-pkg/encoding:utf8"         decString 
    it "jis_x_0208"    $ property $ prop @"enc-pkg/encoding:jis_x_0208"   decString 
    it "koi8_r"        $ property $ prop @"enc-pkg/encoding:koi8_r"       decString
    it "koi8_u"        $ property $ prop @"enc-pkg/encoding:koi8_u"       decString
    it "iso8859_1"     $ property $ prop @"enc-pkg/encoding:iso8859_1"    decString
    it "iso8859_2"     $ property $ prop @"enc-pkg/encoding:iso8859_2"    decString
    it "iso8859_3"     $ property $ prop @"enc-pkg/encoding:iso8859_3"    decString
    it "iso8859_4"     $ property $ prop @"enc-pkg/encoding:iso8859_4"    decString
    it "iso8859_5"     $ property $ prop @"enc-pkg/encoding:iso8859_5"    decString
    it "iso8859_6"     $ property $ prop @"enc-pkg/encoding:iso8859_6"    decString
    it "iso8859_7"     $ property $ prop @"enc-pkg/encoding:iso8859_7"    decString
    it "iso8859_8"     $ property $ prop @"enc-pkg/encoding:iso8859_8"    decString
    it "iso8859_9"     $ property $ prop @"enc-pkg/encoding:iso8859_9"    decString
    it "iso8859_10"    $ property $ prop @"enc-pkg/encoding:iso8859_10"   decString
    it "iso8859_11"    $ property $ prop @"enc-pkg/encoding:iso8859_11"   decString
    it "iso8859_13"    $ property $ prop @"enc-pkg/encoding:iso8859_13"   decString
    it "iso8859_14"    $ property $ prop @"enc-pkg/encoding:iso8859_14"   decString
    it "iso8859_15"    $ property $ prop @"enc-pkg/encoding:iso8859_15"   decString
    it "iso8859_16"    $ property $ prop @"enc-pkg/encoding:iso8859_16"   decString
    it "cp1250"        $ property $ prop @"enc-pkg/encoding:cp1250"       decString
    it "cp1251"        $ property $ prop @"enc-pkg/encoding:cp1251"       decString
    it "cp1252"        $ property $ prop @"enc-pkg/encoding:cp1252"       decString
    it "cp1253"        $ property $ prop @"enc-pkg/encoding:cp1253"       decString
    it "cp1254"        $ property $ prop @"enc-pkg/encoding:cp1254"       decString
    it "cp1255"        $ property $ prop @"enc-pkg/encoding:cp1255"       decString
    it "cp1256"        $ property $ prop @"enc-pkg/encoding:cp1256"       decString
    it "cp1257"        $ property $ prop @"enc-pkg/encoding:cp1257"       decString
    it "cp1258"        $ property $ prop @"enc-pkg/encoding:cp1258"       decString
    it "gb18030"       $ property $ prop @"enc-pkg/encoding:gb18030"      decString
    it "macintosh"     $ property $ prop @"enc-pkg/encoding:macintosh"    decString
    it "jis_x_0201"    $ property $ prop @"enc-pkg/encoding:jis_x_0201"   decString
    it "cp437"         $ property $ prop @"enc-pkg/encoding:cp437"        decString
    it "cp737"         $ property $ prop @"enc-pkg/encoding:cp737"        decString
    it "cp775"         $ property $ prop @"enc-pkg/encoding:cp775"        decString
    it "cp850"         $ property $ prop @"enc-pkg/encoding:cp850"        decString
    it "cp852"         $ property $ prop @"enc-pkg/encoding:cp852"        decString
    it "cp855"         $ property $ prop @"enc-pkg/encoding:cp855"        decString
    it "cp857"         $ property $ prop @"enc-pkg/encoding:cp857"        decString
    it "cp860"         $ property $ prop @"enc-pkg/encoding:cp860"        decString
    it "cp861"         $ property $ prop @"enc-pkg/encoding:cp861"        decString
    it "cp862"         $ property $ prop @"enc-pkg/encoding:cp862"        decString
    it "cp863"         $ property $ prop @"enc-pkg/encoding:cp863"        decString
    it "cp864"         $ property $ prop @"enc-pkg/encoding:cp864"        decString
    it "cp865"         $ property $ prop @"enc-pkg/encoding:cp865"        decString
    it "cp866"         $ property $ prop @"enc-pkg/encoding:cp866"        decString
    it "cp869"         $ property $ prop @"enc-pkg/encoding:cp869"        decString
    it "cp874"         $ property $ prop @"enc-pkg/encoding:cp874"        decString
    
    -- /encoding/ package issue, it allows encoding on invalid character that later fails during decode:
    -- it "cp932"         $ property $ prop @"enc-pkg/encoding:cp932"       encString decString



spec :: Spec 
spec =  
    describe "Sanity" $ do
        describe "save decode after encode" $ 
             suite All (propEncDec encString)
        describe "save decode after validate" $ 
             suite All (propValidation encString validation)

runSpec :: IO ()
runSpec = hspec spec   