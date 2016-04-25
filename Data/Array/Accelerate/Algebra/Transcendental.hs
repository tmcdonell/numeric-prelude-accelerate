{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Algebra.Transcendental
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Algebra.Transcendental (

  module Transcendental

) where

import Algebra.Transcendental                                       as Transcendental
import qualified Data.Array.Accelerate.Algebra.Algebraic            as Algebraic ()

import Data.Array.Accelerate                                        as A


instance C (Exp Float) where
  (**)    = (A.**)
  exp     = A.exp
  log     = A.log
  logBase = A.logBase
  pi      = A.pi
  sin     = A.sin
  cos     = A.cos
  tan     = A.tan
  asin    = A.asin
  acos    = A.acos
  atan    = A.atan
  sinh    = A.sinh
  cosh    = A.cosh
  tanh    = A.tanh
  asinh   = A.asinh
  acosh   = A.acosh
  atanh   = A.atanh

instance C (Exp Double) where
  (**)    = (A.**)
  exp     = A.exp
  log     = A.log
  logBase = A.logBase
  pi      = A.pi
  sin     = A.sin
  cos     = A.cos
  tan     = A.tan
  asin    = A.asin
  acos    = A.acos
  atan    = A.atan
  sinh    = A.sinh
  cosh    = A.cosh
  tanh    = A.tanh
  asinh   = A.asinh
  acosh   = A.acosh
  atanh   = A.atanh

