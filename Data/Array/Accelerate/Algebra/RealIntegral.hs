{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Algebra.RealIntegral
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Algebra.RealIntegral (

  module RealIntegral

) where

import Algebra.RealIntegral                                         as RealIntegral

import qualified Data.Array.Accelerate.Algebra.Absolute             as Absolute ()
import qualified Data.Array.Accelerate.Algebra.IntegralDomain       as IntegralDomain ()
import qualified Data.Array.Accelerate.Algebra.ZeroTestable         as ZeroTestable ()

import Data.Array.Accelerate                                        as A
import Prelude                                                      as P


instance C (Exp Int) where
  quot    = P.quot
  rem     = P.rem
  quotRem = P.quotRem

instance C (Exp Int8) where
  quot    = P.quot
  rem     = P.rem
  quotRem = P.quotRem

instance C (Exp Int16) where
  quot    = P.quot
  rem     = P.rem
  quotRem = P.quotRem

instance C (Exp Int32) where
  quot    = P.quot
  rem     = P.rem
  quotRem = P.quotRem

instance C (Exp Int64) where
  quot    = P.quot
  rem     = P.rem
  quotRem = P.quotRem

instance C (Exp Word) where
  quot    = P.quot
  rem     = P.rem
  quotRem = P.quotRem

instance C (Exp Word8) where
  quot    = P.quot
  rem     = P.rem
  quotRem = P.quotRem

instance C (Exp Word16) where
  quot    = P.quot
  rem     = P.rem
  quotRem = P.quotRem

instance C (Exp Word32) where
  quot    = P.quot
  rem     = P.rem
  quotRem = P.quotRem

instance C (Exp Word64) where
  quot    = P.quot
  rem     = P.rem
  quotRem = P.quotRem

