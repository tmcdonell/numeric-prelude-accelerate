{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Algebra.RealField
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Algebra.RealField (

  module RealField

) where

import Algebra.RealField                                            as RealField
import qualified Data.Array.Accelerate.Algebra.Field                as Field ()
import qualified Data.Array.Accelerate.Algebra.RealRing             as RealRing ()

import Data.Array.Accelerate                                        as A


instance C (Exp Float)
instance C (Exp Double)

