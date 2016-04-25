{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Algebra.Algebraic
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Algebra.Algebraic (

  module Algebraic

) where

import Algebra.Algebraic                                            as Algebraic
import Data.Array.Accelerate.Algebra.Field                          as Field hiding ( C )

import Data.Array.Accelerate                                        as A
import qualified Prelude                                            as P ( fromInteger ) -- should have been re-exported by Accelerate?!?


instance C (Exp Float) where
  sqrt     = A.sqrt
  root n x = x A.** Field.recip (P.fromInteger n)
  x ^/ y   = x A.** fromRational' y

instance C (Exp Double) where
  sqrt     = A.sqrt
  root n x = x A.** Field.recip (P.fromInteger n)
  x ^/ y   = x A.** fromRational' y

