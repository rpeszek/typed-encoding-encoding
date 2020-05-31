
-- | This package is a thin layer over the /encoding/ package to create types compatible
-- with /type-encoding/.
--
-- == Naming conventions
-- 
-- @
-- "enc-pkg/encoding:[encoding]"
-- @ 
--
-- where @"[encoding]"@ is String name used by the 'Encoding.DynEncoding' in the /encoding/ package.
--
-- Example: @"enc-pkg/encoding:cyrillic"@
--
-- == Warnings / Issues
--
-- The /encoding/ package can exhibit a potentially unexpected behavior, examples of this are documented in
--
-- "Data.TypedEncoding.Pkg.Encoding.Conv"
-- 
-- In particular, it seems not possible to implement 
-- 'Data.TypedEncoding.Common.Class.Validate.Validate' instance with a decent performance.
-- 
-- For this reason module defining it is annotated with WARNING pragma and is kept separate:
--
-- "Data.TypedEncoding.Pkg.Encoding.Warn.Instances"  (/typed-encoding/ Validate instance for String encodings)


module Data.TypedEncoding.Pkg.Encoding (
     -- * Type safe versions of encode and decode functions from /encoding/ package
     module Data.TypedEncoding.Pkg.Encoding.Conv
     
     -- * /typed-encoding/ class instances for /Encode/, /Decode/, /ToEncString/, and /FromEncString/ 
     , module Data.TypedEncoding.Pkg.Encoding.Instances
  ) where

import Data.TypedEncoding.Pkg.Encoding.Conv
import Data.TypedEncoding.Pkg.Encoding.Instances      