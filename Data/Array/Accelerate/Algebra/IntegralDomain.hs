{-# LANGUAGE NoImplicitPrelude #-}
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

import Data.Array.Accelerate                                  as A hiding ( even, odd )
import qualified Prelude                                      as P


instance (C a, Ring.C a, IsIntegral a, Elt a) => C (Exp a) where
  divMod = P.divMod


-- | Allows division by zero. If the denominator is zero, the numerator is
-- returned as the remainder.
--
divModZero :: (C a, ZeroTestable.C a, IsIntegral a, Elt a) => Exp a -> Exp a -> Exp (a, a)
divModZero x y =
  isZero y ? ( lift (constant zero, x)
             , lift (divMod x y) )

-- | Test if the first argument evenly divides the second
--
divides :: (C a, ZeroTestable.C a, IsIntegral a, Elt a) => Exp a -> Exp a -> Exp Bool
divides y x = isZero (mod x y)

sameResidueClass :: (C a, ZeroTestable.C a, IsIntegral a, Elt a) => Exp a -> Exp a -> Exp a -> Exp Bool
sameResidueClass m x y = divides m (x-y)

even :: (C a, ZeroTestable.C a, IsIntegral a, Elt a) => Exp a -> Exp Bool
even n = divides 2 n

odd :: (C a, ZeroTestable.C a, IsIntegral a, Elt a) => Exp a -> Exp Bool
odd x = not (even x)

