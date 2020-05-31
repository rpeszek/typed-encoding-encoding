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
module Test.SanitySpec where


import           Data.TypedEncoding.Instances.Restriction.ASCII ()
import           Data.TypedEncoding.Instances.Restriction.UTF8 ()
import qualified Data.TypedEncoding.Instances.Restriction.CHAR8 as CHAR8

import           Data.TypedEncoding 
import           Data.TypedEncoding.Pkg.Encoding.Conv 
import           Data.TypedEncoding.Pkg.Encoding.Instances ()

import           Data.TypedEncoding 

import           Test.QuickCheck.Instances.ByteString ()
import           Test.QuickCheck
import           Test.QuickCheck.Property
import           Test.Hspec
import           Data.Either
import           Data.Char

-- | decoding should never fail if encoding succeeds
propEncDec :: forall s . (Algorithm s "enc-pkg/encoding") => Encoding (Either EncodeEx) s "enc-pkg/encoding" () String -> Decoding (Either UnexpectedDecodeEx) s "enc-pkg/encoding" () String -> String -> Property
propEncDec encf decf str = property $ liftBool (Right enc0 == edec)
   where 
       enc0 = toEncoding () str
       eenc = _runEncoding encf enc0
       edec = case eenc of 
           Right enc -> either (Left . show) Right $ _runDecoding decf enc
           Left enc -> Right enc0 -- magically decode all encoded failures  

data TestSuite = OneBitOnly | All

-- | encoding for one bit encodings should always fail string has chars > '\255'
-- this does not seem to be the case with some encodings like /cp1257/ which could be a bug
-- in encodings library
-- Even adding chars like \1057 (0x421) does not help e.g. /iso8859_5/ encodes it!
--
-- TODO quick and dirty, possibly should not be a property test
propFailOn0x100 :: forall s . (Algorithm s "enc-pkg/encoding") => Encoding (Either EncodeEx) s "enc-pkg/encoding" () String -> Decoding (Either UnexpectedDecodeEx) s "enc-pkg/encoding" () String -> String -> Property
propFailOn0x100 encf decf str = property $ liftBool (isLeft enc)
    where 
        -- try even higher than \x100
        modStr = '\x421' : map upChar str
        enc = _runEncoding encf . toEncoding () $ modStr 

        upChar :: Char -> Char
        upChar ch 
             | ch <= '\xff' = '\x100'
             | otherwise = ch

-- | encoding does not fail on numbers 0-9
-- TODO quick and dirty, not the best to use String as Arbitrary here
propSucceedOnDigits :: forall s . (Algorithm s "enc-pkg/encoding") => Encoding (Either EncodeEx) s "enc-pkg/encoding" () String -> Decoding (Either UnexpectedDecodeEx) s "enc-pkg/encoding" () String -> String -> Property
propSucceedOnDigits encf decf str = property $ liftBool (isRight enc)
    where 
        modStr = map modChars str
        enc = _runEncoding encf . toEncoding () $ modStr 

        modChars :: Char -> Char
        modChars ch 
             | isDigit ch = ch
             | otherwise = '1'

suite :: 
       TestSuite
       -> (forall s . (Algorithm s "enc-pkg/encoding") => Encoding (Either EncodeEx) s "enc-pkg/encoding" () String 
          -> Decoding (Either UnexpectedDecodeEx) s "enc-pkg/encoding" () String 
          -> String 
          -> Property) 
       -> SpecWith ()
suite which prop = do 
    case which of
        All -> do
            it "utf8"          $ property $ prop @"enc-pkg/encoding:utf8"        encString decString 
            it "jis_x_0208"    $ property $ prop @"enc-pkg/encoding:jis_x_0208"  encString decString 
        OneBitOnly ->        
            it "OneBitExecution skipping" $ property $ liftBool True
    it "koi8_r"        $ property $ prop @"enc-pkg/encoding:koi8_r"      encString decString
    it "koi8_u"        $ property $ prop @"enc-pkg/encoding:koi8_u"      encString decString
    it "iso8859_1"     $ property $ prop @"enc-pkg/encoding:iso8859_1"   encString decString
    it "iso8859_2"     $ property $ prop @"enc-pkg/encoding:iso8859_2"   encString decString
    it "iso8859_3"     $ property $ prop @"enc-pkg/encoding:iso8859_3"   encString decString
    it "iso8859_4"     $ property $ prop @"enc-pkg/encoding:iso8859_4"   encString decString
    it "iso8859_5"     $ property $ prop @"enc-pkg/encoding:iso8859_5"   encString decString
    it "iso8859_6"     $ property $ prop @"enc-pkg/encoding:iso8859_6"   encString decString
    it "iso8859_7"     $ property $ prop @"enc-pkg/encoding:iso8859_7"   encString decString
    it "iso8859_8"     $ property $ prop @"enc-pkg/encoding:iso8859_8"   encString decString
    it "iso8859_9"     $ property $ prop @"enc-pkg/encoding:iso8859_9"   encString decString
    it "iso8859_10"    $ property $ prop @"enc-pkg/encoding:iso8859_10"  encString decString
    it "iso8859_11"    $ property $ prop @"enc-pkg/encoding:iso8859_11"  encString decString
    it "iso8859_13"    $ property $ prop @"enc-pkg/encoding:iso8859_13"  encString decString
    it "iso8859_14"    $ property $ prop @"enc-pkg/encoding:iso8859_14"  encString decString
    it "iso8859_15"    $ property $ prop @"enc-pkg/encoding:iso8859_15"  encString decString
    it "iso8859_16"    $ property $ prop @"enc-pkg/encoding:iso8859_16"  encString decString
    it "cp1250"        $ property $ prop @"enc-pkg/encoding:cp1250"      encString decString
    it "cp1251"        $ property $ prop @"enc-pkg/encoding:cp1251"      encString decString
    it "cp1252"        $ property $ prop @"enc-pkg/encoding:cp1252"      encString decString
    it "cp1253"        $ property $ prop @"enc-pkg/encoding:cp1253"      encString decString
    it "cp1254"        $ property $ prop @"enc-pkg/encoding:cp1254"      encString decString
    it "cp1255"        $ property $ prop @"enc-pkg/encoding:cp1255"      encString decString
    it "cp1256"        $ property $ prop @"enc-pkg/encoding:cp1256"      encString decString
    it "cp1257"        $ property $ prop @"enc-pkg/encoding:cp1257"      encString decString
    it "cp1258"        $ property $ prop @"enc-pkg/encoding:cp1258"      encString decString
    it "gb18030"       $ property $ prop @"enc-pkg/encoding:gb18030"     encString decString
    it "macintosh"     $ property $ prop @"enc-pkg/encoding:macintosh"   encString decString
    it "jis_x_0201"    $ property $ prop @"enc-pkg/encoding:jis_x_0201"  encString decString
    it "cp437"         $ property $ prop @"enc-pkg/encoding:cp437"       encString decString
    it "cp737"         $ property $ prop @"enc-pkg/encoding:cp737"       encString decString
    it "cp775"         $ property $ prop @"enc-pkg/encoding:cp775"       encString decString
    it "cp850"         $ property $ prop @"enc-pkg/encoding:cp850"       encString decString
    it "cp852"         $ property $ prop @"enc-pkg/encoding:cp852"       encString decString
    it "cp855"         $ property $ prop @"enc-pkg/encoding:cp855"       encString decString
    it "cp857"         $ property $ prop @"enc-pkg/encoding:cp857"       encString decString
    it "cp860"         $ property $ prop @"enc-pkg/encoding:cp860"       encString decString
    it "cp861"         $ property $ prop @"enc-pkg/encoding:cp861"       encString decString
    it "cp862"         $ property $ prop @"enc-pkg/encoding:cp862"       encString decString
    it "cp863"         $ property $ prop @"enc-pkg/encoding:cp863"       encString decString
    it "cp864"         $ property $ prop @"enc-pkg/encoding:cp864"       encString decString
    it "cp865"         $ property $ prop @"enc-pkg/encoding:cp865"       encString decString
    it "cp866"         $ property $ prop @"enc-pkg/encoding:cp866"       encString decString
    it "cp869"         $ property $ prop @"enc-pkg/encoding:cp869"       encString decString
    it "cp874"         $ property $ prop @"enc-pkg/encoding:cp874"       encString decString
    
    -- /encoding/ package issue, it allows encoding on invalid character that later fails during decode:
    -- it "cp932"         $ property $ prop @"enc-pkg/encoding:cp932"       encString decString



spec :: Spec 
spec =  
    describe "Sanity" $ do
        describe "encode-decode" $ 
             suite All propEncDec

        -- /encoding/ package issue, numerous one-bit encodings encode chars > 255 just fine!
        -- describe "> xFF expect to fail on onebit" $ 
        --      suite OneBitOnly propFailOn0x100
        describe "succed on digits" $
             suite OneBitOnly propSucceedOnDigits  -- fails on jis_x_0208   

runSpec :: IO ()
runSpec = hspec spec   