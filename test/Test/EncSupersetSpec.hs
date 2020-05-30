{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

-- | Verified backward compatibility of ASCII encoder changes in v0.3
module Test.EncSupersetSpec where


import           Data.TypedEncoding.Instances.Restriction.ASCII ()
import           Data.TypedEncoding.Instances.Restriction.UTF8 ()
import qualified Data.TypedEncoding.Instances.Restriction.CHAR8 as CHAR8

import           Data.TypedEncoding.Pkg.Encoding.Conv 
import           Data.TypedEncoding.Pkg.Encoding.Instances ()

import           Data.TypedEncoding 

import           Test.QuickCheck.Instances.ByteString ()
-- import qualified Data.ByteString as B
import           Test.QuickCheck
-- import           Test.QuickCheck.Property
import           Test.Hspec




spec :: Spec 
spec =  
    describe "Superset checks" $ 
        describe "ASCII on String" $ do
            -- it "r-ASCII > ascii" $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-ASCII" @"enc-pkg/encoding:ascii"  encString encoding
            
            -- "r-UTF8" has additional Unicode D76 restriction and hence is not superset 
            -- it "r-UTF8 > utf8"   $ property $ propSuperset_ @"r-UTF8"  @"enc-pkg/encoding:utf8"                encoding encDynB 

            -- "r-CHAR8" tests are useless they will work by desing since all encodings generate <255 byte 
            it "r-CHAR8 > jis_x_0208"         $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:jis_x_0208" encString CHAR8.testEncCHAR8
            it "r-CHAR8 > utf_8"         $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:utf_8" encString CHAR8.testEncCHAR8
            
            -- it "r-CHAR8 > koi8_r"        $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:koi8_r"      encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > koi8_u"        $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:koi8_u"      encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > iso8859_1"     $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:iso8859_1"   encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > iso8859_2"     $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:iso8859_2"   encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > iso8859_3"     $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:iso8859_3"   encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > iso8859_4"     $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:iso8859_4"   encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > iso8859_5"     $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:iso8859_5"   encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > iso8859_6"     $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:iso8859_6"   encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > iso8859_7"     $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:iso8859_7"   encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > iso8859_8"     $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:iso8859_8"   encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > iso8859_9"     $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:iso8859_9"   encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > iso8859_10"    $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:iso8859_10"  encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > iso8859_11"    $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:iso8859_11"  encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > iso8859_13"    $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:iso8859_13"  encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > iso8859_14"    $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:iso8859_14"  encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > iso8859_15"    $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:iso8859_15"  encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > iso8859_16"    $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:iso8859_16"  encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp1250"        $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp1250"      encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp1251"        $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp1251"      encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp1252"        $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp1252"      encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp1253"        $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp1253"      encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp1254"        $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp1254"      encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp1255"        $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp1255"      encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp1256"        $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp1256"      encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp1257"        $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp1257"      encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp1258"        $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp1258"      encString CHAR8.testEncCHAR8
            -- -- it "r-CHAR8 > gb18030"       $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:gb18030"     encString CHAR8.testEncCHAR8
            -- -- it "r-CHAR8 > macintosh"     $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:macintosh"   encString CHAR8.testEncCHAR8
            -- -- it "r-CHAR8 > jis_x_0201"    $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:jis_x_0201"  encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp437"         $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp437"       encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp737"         $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp737"       encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp775"         $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp775"       encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp850"         $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp850"       encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp852"         $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp852"       encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp855"         $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp855"       encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp857"         $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp857"       encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp860"         $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp860"       encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp861"         $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp861"       encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp862"         $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp862"       encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp863"         $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp863"       encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp864"         $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp864"       encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp865"         $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp865"       encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp866"         $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp866"       encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp869"         $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp869"       encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp874"         $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp874"       encString CHAR8.testEncCHAR8
            -- it "r-CHAR8 > cp932"         $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:cp932"       encString CHAR8.testEncCHAR8

runSpec :: IO ()
runSpec = hspec spec   