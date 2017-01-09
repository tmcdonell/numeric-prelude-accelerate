{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Algebra.RealRing
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Algebra.RealRing (

  module RealRing

) where

import Algebra.RealRing                                             as RealRing ( C(..) )
import Data.Array.Accelerate.Algebra.Additive                       as Additive
import Data.Array.Accelerate.Algebra.ToInteger                      as ToInteger
-- import Data.Array.Accelerate.Algebra.Ring                           as Ring
-- import Data.Array.Accelerate.Algebra.Absolute                       as Absolute

import Data.Array.Accelerate                                        ( Exp )

import Prelude                                                      ( (++), ($), Int, Float, Double, String, error )


-- These are likely to fail as they depend on ToInteger.fromIntegral, which only
-- makes sense if GHC rewrite rules fire.
--
-- See [ToIntegral and rewrite rules].

instance RealRing.C (Exp Int) where
  splitFraction x = (fromIntegral x, zero)
  fraction      _ = zero
  floor           = fromIntegral
  ceiling         = fromIntegral
  round           = fromIntegral
  truncate        = fromIntegral

instance RealRing.C (Exp Float) where
  fraction        = npError "fraction"
  floor           = npError "floor"
  ceiling         = npError "ceiling"
  round           = npError "round"
  truncate        = npError "truncate"

instance RealRing.C (Exp Double) where
  fraction        = npError "fraction"
  floor           = npError "floor"
  ceiling         = npError "ceiling"
  round           = npError "round"
  truncate        = npError "truncate"

npError :: String -> a
npError f = error $ "Algebra.RealRing." ++ f ++ ": not implemented yet"



-- fastFraction :: (Ord t, Ring.C (Exp t)) => Exp t -> Exp t
-- fastFraction x
--   = (minBound :: Exp Int64) <=* x &&* x <=* (maxBound :: Exp Int64)
--   ? ( n - A.toFloating f * d
--     , zero -- error...
--     )
  -- This is only valid when minBound <= x <= maxBound
  -- fixFraction (x - trunc x)

-- fixFraction :: (Ord t, Ring.C (Exp t)) => Exp t -> Exp t
-- fixFraction x =
--   x >=* zero ? ( x, x + one )


-- fastSplitFraction
--     :: forall a b. (Ring.C (Exp b), Absolute.C (Exp a), Ord a, Elt a, Elt b)
--     => (Exp a -> Exp Int)
--     -> (Exp Int -> Exp a)
--     -> Exp a
--     -> Exp (b,a)
-- fastSplitFraction trunc toFloat x =
--   let n = trunc x
--       f = x - toFloat n
--       i = fromIntegral n  :: Exp b
--   in
--   f >=* zero ? ( lift (i,     f)     :: Exp (b,a)
--                , lift (i-one, f+one) :: Exp (b,a)
--                )

