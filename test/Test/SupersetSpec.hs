{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

-- | Verified backward compatibility of ASCII encoder changes in v0.3
module Test.SupersetSpec where


import           Data.TypedEncoding.Instances.Restriction.ASCII ()
import           Data.TypedEncoding.Instances.Restriction.UTF8 ()
-- import qualified Data.TypedEncoding.Instances.Restriction.CHAR8 as CHAR8

import           Data.TypedEncoding.Pkg.Instances.Restriction.Encoding 


import           Data.TypedEncoding 

import           Test.QuickCheck.Instances.ByteString ()
-- import qualified Data.ByteString as B
import           Test.QuickCheck
-- import           Test.QuickCheck.Property
import           Test.Hspec

spec :: Spec 
spec =  
    describe "Superset checks" $ 
        describe "ASCII on ByteString" $ do
            it "r-ASCII > ascii" $ property $ propSuperset_ @"r-ASCII" @"r-pkg/encoding:ascii"               encoding encDynS 
            it "r-UTF8 > utf8"   $ property $ propSuperset_ @"r-UTF8"  @"r-pkg/encoding:utf8"                encoding encDynB 
             
            -- checking r-CHAR8  is useless as valid "r-" encoding are 1 byte layouts, encoded means set of char encoded bytes < 255
            -- this is basically true for all encodings

            -- it "r-CHAR8 > wrong"         $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:jis_x_0208"     CHAR8.testEncCHAR8 encDynS 
            -- 
            -- it "r-CHAR8 > koi8_r"        $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:koi8_r"      CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > koi8_u"        $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:koi8_u"      CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > iso8859_1"     $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:iso8859_1"   CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > iso8859_2"     $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:iso8859_2"   CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > iso8859_3"     $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:iso8859_3"   CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > iso8859_4"     $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:iso8859_4"   CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > iso8859_5"     $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:iso8859_5"   CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > iso8859_6"     $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:iso8859_6"   CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > iso8859_7"     $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:iso8859_7"   CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > iso8859_8"     $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:iso8859_8"   CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > iso8859_9"     $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:iso8859_9"   CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > iso8859_10"    $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:iso8859_10"  CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > iso8859_11"    $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:iso8859_11"  CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > iso8859_13"    $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:iso8859_13"  CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > iso8859_14"    $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:iso8859_14"  CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > iso8859_15"    $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:iso8859_15"  CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > iso8859_16"    $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:iso8859_16"  CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp1250"        $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp1250"      CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp1251"        $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp1251"      CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp1252"        $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp1252"      CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp1253"        $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp1253"      CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp1254"        $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp1254"      CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp1255"        $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp1255"      CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp1256"        $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp1256"      CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp1257"        $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp1257"      CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp1258"        $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp1258"      CHAR8.testEncCHAR8 encDynS
            -- -- it "r-CHAR8 > gb18030"       $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:gb18030"     CHAR8.testEncCHAR8 encDynS
            -- -- it "r-CHAR8 > macintosh"     $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:macintosh"   CHAR8.testEncCHAR8 encDynS
            -- -- it "r-CHAR8 > jis_x_0201"    $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:jis_x_0201"  CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp437"         $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp437"       CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp737"         $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp737"       CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp775"         $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp775"       CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp850"         $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp850"       CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp852"         $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp852"       CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp855"         $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp855"       CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp857"         $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp857"       CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp860"         $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp860"       CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp861"         $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp861"       CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp862"         $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp862"       CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp863"         $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp863"       CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp864"         $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp864"       CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp865"         $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp865"       CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp866"         $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp866"       CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp869"         $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp869"       CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp874"         $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp874"       CHAR8.testEncCHAR8 encDynS
            -- it "r-CHAR8 > cp932"         $ property $ propSuperset_ @"r-CHAR8" @"r-pkg/encoding:cp932"       CHAR8.testEncCHAR8 encDynS

runSpec :: IO ()
runSpec = hspec spec   