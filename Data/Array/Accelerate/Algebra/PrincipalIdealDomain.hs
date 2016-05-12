{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Algebra.PrincipalIdealDomain
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Algebra.PrincipalIdealDomain (

  C(..),
  coprime,

  euclid,
  extendedEuclid,

) where


import qualified Data.Array.Accelerate.Algebra.Additive             as Additive
import qualified Data.Array.Accelerate.Algebra.Ring                 as Ring
import qualified Data.Array.Accelerate.Algebra.Units                as Units
import qualified Data.Array.Accelerate.Algebra.ZeroTestable         as ZeroTestable

import Data.Array.Accelerate.Algebra.IntegralDomain                 ( div, mod )
import Data.Array.Accelerate.Algebra.Units                          ( isUnit, stdAssociate )
import Data.Array.Accelerate.Algebra.ZeroTestable                   ( isZero )
import Data.Array.Accelerate.Algebra.Ring                           ( one, (*) )
import Data.Array.Accelerate.Algebra.Additive                       ( zero, (-) )

import Algebra.PrincipalIdealDomain                                 as PID ( C(..) )

import Data.Array.Accelerate                                        as A ( Exp, Elt, while, lift, unlift, not, fst, snd, (?) )
import Data.Array.Accelerate.Smart                                  as A ( tup6, untup6 )
import Data.Array.Accelerate.Type                                   as A


instance C (Exp Int) where
  gcd         = euclid mod
  lcm x y     = isZero x ? ( x, div x (gcd x y) * y )
  extendedGCD = extendedEuclid' div

instance C (Exp Int8) where
  gcd         = euclid mod
  lcm x y     = isZero x ? ( x, div x (gcd x y) * y )
  extendedGCD = extendedEuclid' div

instance C (Exp Int16) where
  gcd         = euclid mod
  lcm x y     = isZero x ? ( x, div x (gcd x y) * y )
  extendedGCD = extendedEuclid' div

instance C (Exp Int32) where
  gcd         = euclid mod
  lcm x y     = isZero x ? ( x, div x (gcd x y) * y )
  extendedGCD = extendedEuclid' div

instance C (Exp Int64) where
  gcd         = euclid mod
  lcm x y     = isZero x ? ( x, div x (gcd x y) * y )
  extendedGCD = extendedEuclid' div


-- | Two integers @a@ and @b@ are coprime (aka: relatively prime, mutually
-- prime) if the only positive integer that divides them both is one.
--
coprime :: (Units.C (Exp a), PID.C (Exp a)) => Exp a -> Exp a -> Exp Bool
coprime a b = isUnit (gcd a b)


-- | An efficient method for computing the greatest common divisor of two
-- numbers.
--
euclid
    :: forall a. (Units.C (Exp a), ZeroTestable.C (Exp a), Elt a)
    => (Exp a -> Exp a -> Exp a)
    -> Exp a
    -> Exp a
    -> Exp a
euclid genMod a b =
  let
      r :: Exp (a,a)
      r = while (\xy -> not (isZero (snd xy)))
                (\xy -> let (x,y) = unlift xy
                            x'    = y
                            y'    = x `genMod` y
                        in
                        lift (x',y'))
                (lift (a,b))
  in
  stdAssociate (fst r)


-- | Implementation of the extended Euclid algorithm which computes, besides the
-- greatest common divisor of integers @a@ and @b@, the coefficients of Bézout's
-- identity @x@ and @y@ such that:
--
-- > ax + by = gcd(a, b)
--
-- https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Pseudocode
--
extendedEuclid
    :: forall a. (Units.C (Exp a), ZeroTestable.C (Exp a), Additive.C (Exp a), Ring.C (Exp a), Elt a)
    => (Exp a -> Exp a -> Exp a)
    -> Exp a
    -> Exp a
    -> Exp (a, (a, a))
extendedEuclid genDiv a b =
  let
      initial :: Exp (a,a,a,a,a,a)
      initial = tup6 ( zero, one  -- s, old_s
                     , one,  zero -- t, old_t
                     , b, a       -- r, old_r
                     )

      aux :: Exp (a,a,a,a,a,a)
      aux = while (\v -> let (_,_,_,_,r,_) = untup6 v
                         in  not (isZero r))
                  (\v -> let (s,old_s,t,old_t,r,old_r) = unlift v
                             q  = old_r `genDiv` r
                             r' = old_r - q * r
                             s' = old_s - q * s
                             t' = old_t - q * t
                         in
                         lift (s',s,t',t,r',r))
                 initial

      (_,x,_,y,_,g) = untup6 aux
  in
  lift (g, (x,y)) -- stdAssociate g ??

extendedEuclid'
    :: forall a. (Units.C (Exp a), ZeroTestable.C (Exp a), Additive.C (Exp a), Ring.C (Exp a), Elt a)
    => (Exp a -> Exp a -> Exp a)
    -> Exp a
    -> Exp a
    -> (Exp a, (Exp a, Exp a))
extendedEuclid' genDiv x y =
  let
      r      = extendedEuclid genDiv x y
      (g,ab) = unlift r   :: (Exp a, Exp (a,a))
      (a,b)  = unlift ab  :: (Exp a, Exp a)
  in
  (g, (a,b))

