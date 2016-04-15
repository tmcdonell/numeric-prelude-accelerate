{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Algebra.Absolute
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Algebra.Absolute (

  module Absolute

) where

import Algebra.Absolute                                             as Absolute
import qualified Data.Array.Accelerate.Algebra.Ring                 as Ring ()

import Data.Array.Accelerate                                        as A
import Prelude                                                      as P


instance C (Exp Int) where
  abs    = P.abs
  signum = P.signum

instance C (Exp Int8) where
  abs    = P.abs
  signum = P.signum

instance C (Exp Int16) where
  abs    = P.abs
  signum = P.signum

instance C (Exp Int32) where
  abs    = P.abs
  signum = P.signum

instance C (Exp Int64) where
  abs    = P.abs
  signum = P.signum

instance C (Exp Word) where
  abs    = P.abs
  signum = P.signum

instance C (Exp Word8) where
  abs    = P.abs
  signum = P.signum

instance C (Exp Word16) where
  abs    = P.abs
  signum = P.signum

instance C (Exp Word32) where
  abs    = P.abs
  signum = P.signum

instance C (Exp Word64) where
  abs    = P.abs
  signum = P.signum

instance C (Exp Float) where
  abs    = P.abs
  signum = P.signum

instance C (Exp Double) where
  abs    = P.abs
  signum = P.signum

