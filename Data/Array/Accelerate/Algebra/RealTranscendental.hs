{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Algebra.RealTranscendental
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Algebra.RealTranscendental (

  module RealTrans

) where

import Algebra.RealTranscendental                                   as RealTrans
import qualified Data.Array.Accelerate.Algebra.RealField            as RealField ()
import qualified Data.Array.Accelerate.Algebra.Transcendental       as Transcendental ()

import Data.Array.Accelerate                                        as A


instance C (Exp Float) where
  atan2 = A.atan2

instance C (Exp Double) where
  atan2 = A.atan2

