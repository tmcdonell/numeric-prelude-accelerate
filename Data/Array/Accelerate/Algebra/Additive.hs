{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Algebra.Additive
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Algebra.Additive (

  module Additive

) where

import Algebra.Additive                                   as Additive

import Data.Array.Accelerate                              as A
import qualified Prelude                                  as P

instance (Additive.C a, IsNum a, Elt a) => C (Exp a) where
  zero   = constant zero
  (+)    = (P.+)
  (-)    = (P.-)
  negate = P.negate

