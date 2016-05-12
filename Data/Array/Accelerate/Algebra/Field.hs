{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Algebra.Field
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Algebra.Field (

  module Field

) where

import Algebra.Field                                                as Field

import qualified Data.Array.Accelerate.Algebra.Ring                 as Ring ()

import Data.Array.Accelerate                                        as A

import Data.Function


instance C (Exp Float) where
  (/)             = (A./)
  recip           = A.recip
  fromRational' x = constant (fromRational' x)

instance C (Exp Double) where
  (/)             = (A./)
  recip           = A.recip
  fromRational' x = constant (fromRational' x)

instance (C (Exp a), C (Exp b), Elt a, Elt b) => C (Exp (a,b)) where
  x / y           = lift (on (Field./) fst x y, on (Field./) snd x y)
  recip x         = lift (Field.recip (fst x), Field.recip (snd x))
  fromRational' x = lift (fromRational' x :: Exp a, fromRational' x :: Exp b)

