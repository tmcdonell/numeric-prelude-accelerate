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
import qualified Prelude                                            as P


instance C (Exp Float) where
  sqrt     = P.sqrt
  root n x = x P.** recip (P.fromInteger n)
  x ^/ y   = x P.** fromRational' y

instance C (Exp Double) where
  sqrt     = P.sqrt
  root n x = x P.** recip (P.fromInteger n)
  x ^/ y   = x P.** fromRational' y

