{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
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

module Data.Array.Accelerate.Number.Complex (

  -- * Cartesian form
  Complex.T, real, imag,

  (+:),
  (-:),
  scale,
  -- exp,
  quarterLeft, quarterRight,

  -- * Polar form
  -- toPolar,
  -- fromPolar,
  -- cis,
  -- signum,
  -- signumNorm,
  -- magnitude,
  magnitudeSqr,
  -- phase,

  -- * Conjugate
  conjugate,

) where

import qualified Number.Complex                           as Complex

import qualified Data.Array.Accelerate.Algebra.Additive   as Additive
import qualified Data.Array.Accelerate.Algebra.Ring       as Ring

import Data.Array.Accelerate                              as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar

import Data.Typeable
import Prelude                                            ( undefined, ($) )


infix 6 +:

-- Cartesian form
-- --------------

-- | Real part
--
real :: Elt a => Exp (Complex.T a) -> Exp a
real = lift1 (Complex.real :: Complex.T (Exp a) -> Exp a)

-- | Imaginary part
imag :: Elt a => Exp (Complex.T a) -> Exp a
imag = lift1 (Complex.imag :: Complex.T (Exp a) -> Exp a)

-- | Construct a complex number from the real and imaginary parts
--
(+:) :: Elt a => Exp a -> Exp a -> Exp (Complex.T a)
(+:) = lift2 ((Complex.+:) :: Exp a -> Exp a -> Complex.T (Exp a))

-- | Construct a complex number with negated imaginary part
--
(-:) :: forall a. (Elt a, Additive.C (Exp a)) => Exp a -> Exp a -> Exp (Complex.T a)
(-:) = lift2 ((Complex.-:) :: Exp a -> Exp a -> Complex.T (Exp a))

-- | Scale a complex number by a real number.
--
scale :: forall a. (Ring.C (Exp a), Elt a) => Exp a -> Exp (Complex.T a) -> Exp (Complex.T a)
scale = lift2 (Complex.scale :: Exp a -> Complex.T (Exp a) -> Complex.T (Exp a))

-- | Exponential of a complex number with minimal type class constraints.
--
-- exp :: (Trans.C a) => T a -> T a
-- exp (Cons x y) =  scale (Trans.exp x) (cis y)

-- | Turn the point one quarter to the right.
--
quarterRight :: forall a. (Additive.C (Exp a), Elt a) => Exp (Complex.T a) -> Exp (Complex.T a)
quarterRight = lift1 (Complex.quarterRight :: Complex.T (Exp a) -> Complex.T (Exp a))

-- | Turn the point one quarter to the left
--
quarterLeft :: forall a. (Additive.C (Exp a), Elt a) => Exp (Complex.T a) -> Exp (Complex.T a)
quarterLeft = lift1 (Complex.quarterLeft :: Complex.T (Exp a) -> Complex.T (Exp a))

-- Polar form
-- ----------

-- | The function 'toPolar' takes a complex number and returns a (magnitude,
-- phase) pair in canonical form: the magnitude is non-negative, and the phase
-- in the range @(-'pi', 'pi']@; if the magnitude is zero, then so is the phase.
--
-- toPolar :: (RealTrans.C a, ZeroTestable.C a) => T a -> (a,a)
-- toPolar z = (magnitude z, phase z)

-- | Form a complex number from polar components of magnitude and phase.
--
-- fromPolar :: (Trans.C a) => a -> a -> T a
-- fromPolar r theta =  scale r (cis theta)

-- | @'cis' t@ is a complex value with magnitude @1@ and phase @t@ (modulo
-- @2*'pi'@).
--
-- cis :: (Trans.C a) => a -> T a
-- cis theta =  Cons (cos theta) (sin theta)

-- | The non-negative magnitude of a complex number. This implementation
-- respects the limited range of floating point numbers. The trivial
-- implementation 'magnitude' would overflow for floating point exponents
-- greater than the half of the maximum admissible exponent.
--
-- floatMagnitude :: (P.RealFloat a, Algebraic.C a) => T a -> a
-- floatMagnitude (Cons x y) =
--    let k  = max (P.exponent x) (P.exponent y)
--        mk = - k
--    in  P.scaleFloat k
--            (sqrt (P.scaleFloat mk x ^ 2 +
--                   P.scaleFloat mk y ^ 2))

-- | The non-negative magnitude of a complex number.
--
-- magnitude :: (Algebraic.C a) => T a -> a
-- magnitude = sqrt . magnitudeSqr

-- | Like NormedEuc.normSqr with lifted class constraints
--
magnitudeSqr :: forall a. (Ring.C (Exp a), Elt a) => Exp (Complex.T a) -> Exp a
magnitudeSqr = lift1 (Complex.magnitudeSqr :: Complex.T (Exp a) -> Exp a)

-- | The phase of a complex number, in the range @(-'pi', 'pi']@. If the
-- magnitude is zero, then so is the phase.
--
-- phase :: (RealTrans.C a, ZeroTestable.C a) => T a -> a
-- phase z =
--    if isZero z
--      then zero   -- SLPJ July 97 from John Peterson
--      else case z of (Cons x y) -> atan2 y x

-- | Scale a complex number to magnitude 1.
--
-- For a complex number @z@, @'abs' z@ is a number with the magnitude of @z@,
-- but oriented in the positive real direction, whereas @'signum' z@ has the
-- phase of @z@, but unit magnitude.
--
-- signum :: (Algebraic.C a, ZeroTestable.C a) => T a -> T a
-- signum z =
--    if isZero z
--      then zero
--      else scale (recip (magnitude z)) z

-- signumNorm :: (Algebraic.C a, NormedEuc.C a a, ZeroTestable.C a) => T a -> T a
-- signumNorm z =
--    if isZero z
--      then zero
--      else scale (recip (NormedEuc.norm z)) z

-- Conjugate
-- ---------

-- | The conjugate of a complex number.
--
conjugate :: forall a. (Additive.C (Exp a), Elt a) => Exp (Complex.T a) -> Exp (Complex.T a)
conjugate = lift1 (Complex.conjugate :: Complex.T (Exp a) -> Complex.T (Exp a))


-- Accelerate classes
-- ------------------

#if __GLASGOW_HASKELL__ <= 708
deriving instance Typeable Complex.T
#endif

type instance EltRepr (Complex.T a) = EltRepr (a, a)

instance Elt a => Elt (Complex.T a) where
  eltType _   = eltType (undefined :: (a,a))
  toElt p     = let (a, b) = toElt p in a Complex.+: b
  fromElt c   = fromElt (Complex.real c, Complex.imag c)

instance cst a => IsProduct cst (Complex.T a) where
  type ProdRepr (Complex.T a) = ProdRepr (a, a)
  fromProd cst c  = fromProd cst (Complex.real c, Complex.imag c)
  toProd cst p    = let (x, y) = toProd cst p in x Complex.+: y
  prod cst _      = prod cst (undefined :: (a, a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Complex.T a) where
  type Plain (Complex.T a) = Complex.T (Plain a)
  lift c                   = Exp $ Tuple (NilTup `SnocTup` lift (Complex.real c)
                                                 `SnocTup` lift (Complex.imag c))

instance Elt a => Unlift Exp (Complex.T (Exp a)) where
  unlift e =
    let x = Exp $ SuccTupIdx ZeroTupIdx `Prj` e
        y = Exp $ ZeroTupIdx `Prj` e
    in
    x Complex.+: y

