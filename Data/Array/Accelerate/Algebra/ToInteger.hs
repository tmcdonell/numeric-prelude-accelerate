{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Algebra.ToInteger
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Vacuous implementation of the numeric-prelude 'ToInteger' module. The
-- NP.fromIntegral function is completely untenable for Accelerate, but maybe we
-- can get around that sometimes with rewrite rules.
--

module Data.Array.Accelerate.Algebra.ToInteger (

  module ToInteger,

) where

import Algebra.ToInteger                                            as ToInteger

import qualified Data.Array.Accelerate.Algebra.RealIntegral         as RealIntegral ()
import qualified Data.Array.Accelerate.Algebra.ToRational           as ToRational ()

import Data.Array.Accelerate                                        as A
import Prelude                                                      as P ( ($), Integer, error, unlines )


instance C (Exp Int)    where toInteger = npToIntegerError
instance C (Exp Int8)   where toInteger = npToIntegerError
instance C (Exp Int16)  where toInteger = npToIntegerError
instance C (Exp Int32)  where toInteger = npToIntegerError
instance C (Exp Int64)  where toInteger = npToIntegerError

instance C (Exp Word)   where toInteger = npToIntegerError
instance C (Exp Word8)  where toInteger = npToIntegerError
instance C (Exp Word16) where toInteger = npToIntegerError
instance C (Exp Word32) where toInteger = npToIntegerError
instance C (Exp Word64) where toInteger = npToIntegerError

npToIntegerError :: a -> Integer
npToIntegerError
  = error
  $ unlines [ "Algebra.ToInteger.toInteger is incompatible with Accelerate"
            , "and GHC RULES failed to rewrite this into something sensible"
            ]

-- Note: [ToIntegral and rewrite rules]
--
-- Since numeric prelude forces the implementation of fromIntegral to go via
-- 'ToInteger', the only way for 'fromIntegral' to make sense for Accelerate is
-- if the following rewrite rules fire.
--
{-# RULES
     "NP.fromIntegral :: Exp Int     -> Exp Int"     ToInteger.fromIntegral = A.fromIntegral :: Exp Int     -> Exp Int;
     "NP.fromIntegral :: Exp Int     -> Exp Float"   ToInteger.fromIntegral = A.fromIntegral :: Exp Int     -> Exp Float;
     "NP.fromIntegral :: Exp Int     -> Exp Double"  ToInteger.fromIntegral = A.fromIntegral :: Exp Int     -> Exp Double;
     "NP.fromIntegral :: Exp Int8    -> Exp Int"     ToInteger.fromIntegral = A.fromIntegral :: Exp Int8    -> Exp Int;
     "NP.fromIntegral :: Exp Int8    -> Exp Float"   ToInteger.fromIntegral = A.fromIntegral :: Exp Int8    -> Exp Float;
     "NP.fromIntegral :: Exp Int8    -> Exp Double"  ToInteger.fromIntegral = A.fromIntegral :: Exp Int8    -> Exp Double;
     "NP.fromIntegral :: Exp Int16   -> Exp Int"     ToInteger.fromIntegral = A.fromIntegral :: Exp Int16   -> Exp Int;
     "NP.fromIntegral :: Exp Int16   -> Exp Float"   ToInteger.fromIntegral = A.fromIntegral :: Exp Int16   -> Exp Float;
     "NP.fromIntegral :: Exp Int16   -> Exp Double"  ToInteger.fromIntegral = A.fromIntegral :: Exp Int16   -> Exp Double;
     "NP.fromIntegral :: Exp Int32   -> Exp Int"     ToInteger.fromIntegral = A.fromIntegral :: Exp Int32   -> Exp Int;
     "NP.fromIntegral :: Exp Int32   -> Exp Float"   ToInteger.fromIntegral = A.fromIntegral :: Exp Int32   -> Exp Float;
     "NP.fromIntegral :: Exp Int32   -> Exp Double"  ToInteger.fromIntegral = A.fromIntegral :: Exp Int32   -> Exp Double;
     "NP.fromIntegral :: Exp Int64   -> Exp Int"     ToInteger.fromIntegral = A.fromIntegral :: Exp Int64   -> Exp Int;
     "NP.fromIntegral :: Exp Int64   -> Exp Float"   ToInteger.fromIntegral = A.fromIntegral :: Exp Int64   -> Exp Float;
     "NP.fromIntegral :: Exp Int64   -> Exp Double"  ToInteger.fromIntegral = A.fromIntegral :: Exp Int64   -> Exp Double;
     "NP.fromIntegral :: Exp Word    -> Exp Int"     ToInteger.fromIntegral = A.fromIntegral :: Exp Word    -> Exp Int;
     "NP.fromIntegral :: Exp Word    -> Exp Float"   ToInteger.fromIntegral = A.fromIntegral :: Exp Word    -> Exp Float;
     "NP.fromIntegral :: Exp Word    -> Exp Double"  ToInteger.fromIntegral = A.fromIntegral :: Exp Word    -> Exp Double;
     "NP.fromIntegral :: Exp Word8   -> Exp Int"     ToInteger.fromIntegral = A.fromIntegral :: Exp Word8   -> Exp Int;
     "NP.fromIntegral :: Exp Word8   -> Exp Float"   ToInteger.fromIntegral = A.fromIntegral :: Exp Word8   -> Exp Float;
     "NP.fromIntegral :: Exp Word8   -> Exp Double"  ToInteger.fromIntegral = A.fromIntegral :: Exp Word8   -> Exp Double;
     "NP.fromIntegral :: Exp Word16  -> Exp Int"     ToInteger.fromIntegral = A.fromIntegral :: Exp Word16  -> Exp Int;
     "NP.fromIntegral :: Exp Word16  -> Exp Float"   ToInteger.fromIntegral = A.fromIntegral :: Exp Word16  -> Exp Float;
     "NP.fromIntegral :: Exp Word16  -> Exp Double"  ToInteger.fromIntegral = A.fromIntegral :: Exp Word16  -> Exp Double;
     "NP.fromIntegral :: Exp Word32  -> Exp Int"     ToInteger.fromIntegral = A.fromIntegral :: Exp Word32  -> Exp Int;
     "NP.fromIntegral :: Exp Word32  -> Exp Float"   ToInteger.fromIntegral = A.fromIntegral :: Exp Word32  -> Exp Float;
     "NP.fromIntegral :: Exp Word32  -> Exp Double"  ToInteger.fromIntegral = A.fromIntegral :: Exp Word32  -> Exp Double;
     "NP.fromIntegral :: Exp Word64  -> Exp Int"     ToInteger.fromIntegral = A.fromIntegral :: Exp Word64  -> Exp Int;
     "NP.fromIntegral :: Exp Word64  -> Exp Float"   ToInteger.fromIntegral = A.fromIntegral :: Exp Word64  -> Exp Float;
     "NP.fromIntegral :: Exp Word64  -> Exp Double"  ToInteger.fromIntegral = A.fromIntegral :: Exp Word64  -> Exp Double;
  #-}

