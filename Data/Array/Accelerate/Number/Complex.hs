{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Number.Complex
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Complex numbers
--

module Data.Array.Accelerate.Number.Complex
  where

import Number.Complex                                     as Complex

import Data.Array.Accelerate                              as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar

import Prelude                                            ( undefined, ($) )


type instance EltRepr (Complex.T a) = EltRepr (a, a)

instance Elt a => Elt (Complex.T a) where
  eltType _   = eltType (undefined :: (a,a))
  toElt p     = let (a, b) = toElt p in a +: b
  fromElt c   = fromElt (real c, imag c)

instance cst a => IsProduct cst (Complex.T a) where
  type ProdRepr (Complex.T a) = ProdRepr (a, a)
  fromProd cst c  = fromProd cst (real c, imag c)
  toProd cst p    = let (x, y) = toProd cst p in x +: y
  prod cst _      = prod cst (undefined :: (a, a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Complex.T a) where
  type Plain (Complex.T a) = Complex.T (Plain a)
  lift c                   = Exp $ Tuple (NilTup `SnocTup` lift (real c) `SnocTup` lift (imag c))

instance Elt a => Unlift Exp (Complex.T (Exp a)) where
  unlift e =
    let x = Exp $ SuccTupIdx ZeroTupIdx `Prj` e
        y = Exp $ ZeroTupIdx `Prj` e
    in
    x +: y

