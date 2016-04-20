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
import qualified Prelude                                            as P


instance C (Exp Float) where
  (**)    = (P.**)
  exp     = P.exp
  log     = P.log
  logBase = P.logBase
  pi      = P.pi
  sin     = P.sin
  cos     = P.cos
  tan     = P.tan
  asin    = P.asin
  acos    = P.acos
  atan    = P.atan
  sinh    = P.sinh
  cosh    = P.cosh
  tanh    = P.tanh
  asinh   = P.asinh
  acosh   = P.acosh
  atanh   = P.atanh

instance C (Exp Double) where
  (**)    = (P.**)
  exp     = P.exp
  log     = P.log
  logBase = P.logBase
  pi      = P.pi
  sin     = P.sin
  cos     = P.cos
  tan     = P.tan
  asin    = P.asin
  acos    = P.acos
  atan    = P.atan
  sinh    = P.sinh
  cosh    = P.cosh
  tanh    = P.tanh
  asinh   = P.asinh
  acosh   = P.acosh
  atanh   = P.atanh

