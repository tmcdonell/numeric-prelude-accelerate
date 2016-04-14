{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Algebra.Ring
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Algebra.Ring (

  module Ring

) where

import Algebra.Ring                                       as Ring
import qualified Data.Array.Accelerate.Algebra.Additive   as Additive ()

import Data.Array.Accelerate                              as A
import qualified Prelude                                  as P


-- instance (C a, Additive.C a, IsNum a, Elt a) => C (Exp a) where
--   one           = constant one
--   (*)           = (P.*)
--   fromInteger x = constant (fromInteger x)

instance C (Exp Int) where
  one           = constant one
  (*)           = (P.*)
  fromInteger x = constant (fromInteger x)

instance C (Exp Int8) where
  one           = constant one
  (*)           = (P.*)
  fromInteger x = constant (fromInteger x)

instance C (Exp Int16) where
  one           = constant one
  (*)           = (P.*)
  fromInteger x = constant (fromInteger x)

instance C (Exp Int32) where
  one           = constant one
  (*)           = (P.*)
  fromInteger x = constant (fromInteger x)

instance C (Exp Int64) where
  one           = constant one
  (*)           = (P.*)
  fromInteger x = constant (fromInteger x)

instance C (Exp Word) where
  one           = constant one
  (*)           = (P.*)
  fromInteger x = constant (fromInteger x)

instance C (Exp Word8) where
  one           = constant one
  (*)           = (P.*)
  fromInteger x = constant (fromInteger x)

instance C (Exp Word16) where
  one           = constant one
  (*)           = (P.*)
  fromInteger x = constant (fromInteger x)

instance C (Exp Word32) where
  one           = constant one
  (*)           = (P.*)
  fromInteger x = constant (fromInteger x)

instance C (Exp Word64) where
  one           = constant one
  (*)           = (P.*)
  fromInteger x = constant (fromInteger x)

instance C (Exp Float) where
  one           = constant one
  (*)           = (P.*)
  fromInteger x = constant (fromInteger x)

instance C (Exp Double) where
  one           = constant one
  (*)           = (P.*)
  fromInteger x = constant (fromInteger x)

