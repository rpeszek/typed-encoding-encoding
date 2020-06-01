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


import           Data.TypedEncoding 
import           Data.TypedEncoding.Pkg.Encoding.Conv 
import           Data.TypedEncoding.Pkg.Encoding.Instances ()

import           Data.TypedEncoding 

import           Test.QuickCheck.Instances.ByteString ()
import           Test.Hspec
import           Data.Either

-- OneBitProblems are one bit encodings that somehow can encode chars > '\255'
data TestSuite = MultiBit | OneBit | OneBitProblems 

-- | encoding for one bit encodings should always fail string has chars > '\255'
-- this does not seem to be the case with some encodings like /cp1257/ which could be a bug
-- in encodings library
-- Even adding chars like \1057 (0x421) does not help e.g. /iso8859_5/ encodes it!
--
expectToFailEnc :: forall s . (Algorithm s "enc-pkg/encoding") => String -> Encoding (Either EncodeEx) s "enc-pkg/encoding" () String -> Decoding (Either UnexpectedDecodeEx) s "enc-pkg/encoding" () String  -> Bool
expectToFailEnc str encf decf = isLeft enc
    where 
        enc = _runEncoding encf . toEncoding () $ str 

 
-- | encoding does not fail on numbers 0-9
expectSucceedEnc :: forall s . (Algorithm s "enc-pkg/encoding") =>  String -> Encoding (Either EncodeEx) s "enc-pkg/encoding" () String -> Decoding (Either UnexpectedDecodeEx) s "enc-pkg/encoding" () String -> Bool
expectSucceedEnc str encf decf = isRight enc
     where 
        enc = _runEncoding encf . toEncoding () $ str 

suite :: 
       TestSuite
       -> (forall s . (Algorithm s "enc-pkg/encoding") => 
          Encoding (Either EncodeEx) s "enc-pkg/encoding" () String 
          -> Decoding (Either UnexpectedDecodeEx) s "enc-pkg/encoding" () String 
          -> Bool) 
       -> SpecWith (Arg Bool)
suite which test = 
    case which of
        MultiBit -> do
            it "utf8"          $ test @"enc-pkg/encoding:utf8"        encString decString 
            it "jis_x_0208"    $ test @"enc-pkg/encoding:jis_x_0208"  encString decString 
        OneBitProblems -> do
            it "iso8859_4"     $ test @"enc-pkg/encoding:iso8859_4"   encString decString
            it "iso8859_10"    $ test @"enc-pkg/encoding:iso8859_10"  encString decString
            it "iso8859_13"    $ test @"enc-pkg/encoding:iso8859_13"  encString decString
            it "cp1257"        $ test @"enc-pkg/encoding:cp1257"      encString decString
            it "gb18030"       $ test @"enc-pkg/encoding:gb18030"     encString decString
            it "gb18030"       $ test @"enc-pkg/encoding:gb18030"     encString decString
        OneBit -> do      
            it "koi8_r"        $ test @"enc-pkg/encoding:koi8_r"      encString decString
            it "koi8_u"        $ test @"enc-pkg/encoding:koi8_u"      encString decString
            it "iso8859_1"     $ test @"enc-pkg/encoding:iso8859_1"   encString decString
            it "iso8859_2"     $ test @"enc-pkg/encoding:iso8859_2"   encString decString
            it "iso8859_3"     $ test @"enc-pkg/encoding:iso8859_3"   encString decString
            it "iso8859_5"     $ test @"enc-pkg/encoding:iso8859_5"   encString decString
            it "iso8859_6"     $ test @"enc-pkg/encoding:iso8859_6"   encString decString
            it "iso8859_7"     $ test @"enc-pkg/encoding:iso8859_7"   encString decString
            it "iso8859_8"     $ test @"enc-pkg/encoding:iso8859_8"   encString decString
            it "iso8859_9"     $ test @"enc-pkg/encoding:iso8859_9"   encString decString
            it "iso8859_11"    $ test @"enc-pkg/encoding:iso8859_11"  encString decString
            it "iso8859_14"    $ test @"enc-pkg/encoding:iso8859_14"  encString decString
            it "iso8859_15"    $ test @"enc-pkg/encoding:iso8859_15"  encString decString
            it "iso8859_16"    $ test @"enc-pkg/encoding:iso8859_16"  encString decString
            it "cp1250"        $ test @"enc-pkg/encoding:cp1250"      encString decString
            it "cp1251"        $ test @"enc-pkg/encoding:cp1251"      encString decString
            it "cp1252"        $ test @"enc-pkg/encoding:cp1252"      encString decString
            it "cp1253"        $ test @"enc-pkg/encoding:cp1253"      encString decString
            it "cp1254"        $ test @"enc-pkg/encoding:cp1254"      encString decString
            it "cp1255"        $ test @"enc-pkg/encoding:cp1255"      encString decString
            it "cp1256"        $ test @"enc-pkg/encoding:cp1256"      encString decString
            it "cp1258"        $ test @"enc-pkg/encoding:cp1258"      encString decString
            it "macintosh"     $ test @"enc-pkg/encoding:macintosh"   encString decString
            it "jis_x_0201"    $ test @"enc-pkg/encoding:jis_x_0201"  encString decString
            it "cp437"         $ test @"enc-pkg/encoding:cp437"       encString decString
            it "cp737"         $ test @"enc-pkg/encoding:cp737"       encString decString
            it "cp850"         $ test @"enc-pkg/encoding:cp850"       encString decString
            it "cp852"         $ test @"enc-pkg/encoding:cp852"       encString decString
            it "cp855"         $ test @"enc-pkg/encoding:cp855"       encString decString
            it "cp857"         $ test @"enc-pkg/encoding:cp857"       encString decString
            it "cp860"         $ test @"enc-pkg/encoding:cp860"       encString decString
            it "cp861"         $ test @"enc-pkg/encoding:cp861"       encString decString
            it "cp862"         $ test @"enc-pkg/encoding:cp862"       encString decString
            it "cp863"         $ test @"enc-pkg/encoding:cp863"       encString decString
            it "cp864"         $ test @"enc-pkg/encoding:cp864"       encString decString
            it "cp865"         $ test @"enc-pkg/encoding:cp865"       encString decString
            it "cp866"         $ test @"enc-pkg/encoding:cp866"       encString decString
            it "cp869"         $ test @"enc-pkg/encoding:cp869"       encString decString
            it "cp874"         $ test @"enc-pkg/encoding:cp874"       encString decString           
            -- /encoding/ package issue, it allows encoding on invalid character that later fails during decode:
            it "cp932"         $ test @"enc-pkg/encoding:cp932"       encString decString



spec :: Spec 
spec =  
    describe "Sanity" $ do
 
        -- /encoding/ package issue, numerous one-bit encodings encode chars > 255 just fine!
        describe "> xFF expect to fail on onebit" $ 
             suite OneBit $ expectToFailEnc "\x100"
        describe "> xFF expect to fail on onebit" $ 
             suite OneBitProblems $ fmap not . expectToFailEnc "\x100"
        describe "succeed on digit 1" $
             suite OneBit $ expectSucceedEnc "1"  -- fails on jis_x_0208   
        describe "succeed on digit 1" $
             suite OneBitProblems $ expectSucceedEnc "1"     

runSpec :: IO ()
runSpec = hspec spec   