{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Algebra.ToRational
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Algebra.ToRational (

  module ToRational,

) where

import Algebra.ToRational                                           as ToRational
import Number.Ratio

import qualified Data.Array.Accelerate.Algebra.Absolute             as Absolute ()
import qualified Data.Array.Accelerate.Algebra.ZeroTestable         as ZeroTestable ()

import Data.Array.Accelerate                                        as A
import Prelude                                                      as P ( ($), error, unlines )


instance C (Exp Int)    where toRational = npToRationalError
instance C (Exp Int8)   where toRational = npToRationalError
instance C (Exp Int16)  where toRational = npToRationalError
instance C (Exp Int32)  where toRational = npToRationalError
instance C (Exp Int64)  where toRational = npToRationalError

instance C (Exp Word)   where toRational = npToRationalError
instance C (Exp Word8)  where toRational = npToRationalError
instance C (Exp Word16) where toRational = npToRationalError
instance C (Exp Word32) where toRational = npToRationalError
instance C (Exp Word64) where toRational = npToRationalError

npToRationalError :: a -> Rational
npToRationalError
  = error
  $ unlines [ "Algebra.ToRational.toRational is incompatible with Accelerate" ]

-- TLM: These rule probably won't fire, but they are the only mechanism we have
--      for NP's 'realToField' to do anything useful in Accelerate.
--
{-# RULES
     "NP.realToField :: Exp Int    -> Exp Float"   realToField = A.toFloating :: Exp Int    -> Exp Float ;
     "NP.realToField :: Exp Int8   -> Exp Float"   realToField = A.toFloating :: Exp Int8   -> Exp Float ;
     "NP.realToField :: Exp Int16  -> Exp Float"   realToField = A.toFloating :: Exp Int16  -> Exp Float ;
     "NP.realToField :: Exp Int32  -> Exp Float"   realToField = A.toFloating :: Exp Int32  -> Exp Float ;
     "NP.realToField :: Exp Int64  -> Exp Float"   realToField = A.toFloating :: Exp Int64  -> Exp Float ;
     "NP.realToField :: Exp Word   -> Exp Float"   realToField = A.toFloating :: Exp Word   -> Exp Float ;
     "NP.realToField :: Exp Word8  -> Exp Float"   realToField = A.toFloating :: Exp Word8  -> Exp Float ;
     "NP.realToField :: Exp Word16 -> Exp Float"   realToField = A.toFloating :: Exp Word16 -> Exp Float ;
     "NP.realToField :: Exp Word32 -> Exp Float"   realToField = A.toFloating :: Exp Word32 -> Exp Float ;
     "NP.realToField :: Exp Word64 -> Exp Float"   realToField = A.toFloating :: Exp Word64 -> Exp Float ;
     "NP.realToField :: Exp Float  -> Exp Float"   realToField = A.toFloating :: Exp Float  -> Exp Float ;
     "NP.realToField :: Exp Double -> Exp Float"   realToField = A.toFloating :: Exp Double -> Exp Float ;
     "NP.realToField :: Exp Int    -> Exp Double"  realToField = A.toFloating :: Exp Int    -> Exp Double;
     "NP.realToField :: Exp Int8   -> Exp Double"  realToField = A.toFloating :: Exp Int8   -> Exp Double;
     "NP.realToField :: Exp Int16  -> Exp Double"  realToField = A.toFloating :: Exp Int16  -> Exp Double;
     "NP.realToField :: Exp Int32  -> Exp Double"  realToField = A.toFloating :: Exp Int32  -> Exp Double;
     "NP.realToField :: Exp Int64  -> Exp Double"  realToField = A.toFloating :: Exp Int64  -> Exp Double;
     "NP.realToField :: Exp Word   -> Exp Double"  realToField = A.toFloating :: Exp Word   -> Exp Double;
     "NP.realToField :: Exp Word8  -> Exp Double"  realToField = A.toFloating :: Exp Word8  -> Exp Double;
     "NP.realToField :: Exp Word16 -> Exp Double"  realToField = A.toFloating :: Exp Word16 -> Exp Double;
     "NP.realToField :: Exp Word32 -> Exp Double"  realToField = A.toFloating :: Exp Word32 -> Exp Double;
     "NP.realToField :: Exp Word64 -> Exp Double"  realToField = A.toFloating :: Exp Word64 -> Exp Double;
     "NP.realToField :: Exp Float  -> Exp Double"  realToField = A.toFloating :: Exp Float  -> Exp Double;
     "NP.realToField :: Exp Double -> Exp Double"  realToField = A.toFloating :: Exp Double -> Exp Double;
  #-}

