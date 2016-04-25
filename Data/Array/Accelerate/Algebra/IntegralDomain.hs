{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Algebra.IntegralDomain
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Algebra.IntegralDomain (

  module IntegralDomain,
  divModZero, divides, sameResidueClass, even, odd,

) where

import Algebra.IntegralDomain                                 as IntegralDomain hiding ( divModZero, divides, sameResidueClass, even, odd )

import Data.Array.Accelerate.Algebra.Additive                 ( zero, (-) )
import Data.Array.Accelerate.Algebra.ZeroTestable             ( isZero )
import qualified Data.Array.Accelerate.Algebra.ZeroTestable   as ZeroTestable
import qualified Data.Array.Accelerate.Algebra.Ring           as Ring

import Data.Bool
import Data.Int
import Data.Word

import Data.Array.Accelerate                                  ( Exp, Elt, lift, (?) )
import qualified Data.Array.Accelerate                        as A


-- instance (C a, Ring.C a, Integral a, Elt a) => C (Exp a) where
--   divMod = P.divMod


-- | Allows division by zero. If the denominator is zero, the numerator is
-- returned as the remainder.
--
divModZero
    :: forall a. (C (Exp a), ZeroTestable.C (Exp a), Elt a)
    => Exp a
    -> Exp a
    -> Exp (a, a)
divModZero x y =
  isZero y ? ( lift (zero :: Exp a, x)
             , lift (divMod x y) )

-- | Test if the first argument evenly divides the second
--
divides
    :: (C (Exp a), ZeroTestable.C (Exp a), Elt a)
    => Exp a
    -> Exp a
    -> Exp Bool
divides y x = isZero (mod x y)

sameResidueClass
    :: (C (Exp a), ZeroTestable.C (Exp a), Elt a)
    => Exp a
    -> Exp a
    -> Exp a
    -> Exp Bool
sameResidueClass m x y = divides m (x-y)

even :: (C (Exp a), ZeroTestable.C (Exp a), Elt a)
     => Exp a
     -> Exp Bool
even n = divides (Ring.fromInteger 2) n

odd :: (C (Exp a), ZeroTestable.C (Exp a), Elt a)
    => Exp a
    -> Exp Bool
odd x = A.not (even x)


instance C (Exp Int) where
  divMod = A.divMod

instance C (Exp Int8) where
  divMod = A.divMod

instance C (Exp Int16) where
  divMod = A.divMod

instance C (Exp Int32) where
  divMod = A.divMod

instance C (Exp Int64) where
  divMod = A.divMod

instance C (Exp Word) where
  divMod = A.divMod

instance C (Exp Word8) where
  divMod = A.divMod

instance C (Exp Word16) where
  divMod = A.divMod

instance C (Exp Word32) where
  divMod = A.divMod

instance C (Exp Word64) where
  divMod = A.divMod

