{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Algebra.Module
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Algebra.Module (

  module Module

) where


import Algebra.Module                                               as Module
import qualified Data.Array.Accelerate.Algebra.Additive             as Additive ()
import qualified Data.Array.Accelerate.Algebra.Ring                 as Ring ()

import Data.Array.Accelerate                                        as A


instance Module.C (Exp Int) (Exp Int) where
  (*>) = (*)

instance Module.C (Exp Float) (Exp Float) where
  (*>) = (*)

instance Module.C (Exp Double) (Exp Double) where
  (*>) = (*)

