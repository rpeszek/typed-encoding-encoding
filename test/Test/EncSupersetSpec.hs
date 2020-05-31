{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

-- | 
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



-- | "r-CHAR8" tests are useless they will work by design since all encodings generate <255 byte 
spec :: Spec 
spec =  
    describe "Superset checks" $ 
        describe "ASCII on String" $ do
             it "r-CHAR8 > utf_8"         $ property $ propEncodesInto' @"enc-pkg/encoding" @"r-CHAR8" @"enc-pkg/encoding:utf_8" encString CHAR8.testEncCHAR8

 
runSpec :: IO ()
runSpec = hspec spec   