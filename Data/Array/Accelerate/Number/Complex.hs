{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
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
  exp,
  quarterLeft, quarterRight,

  -- * Polar form
  toPolar,
  fromPolar,
  cis,
  signum,
  -- signumNorm,
  magnitude,
  magnitudeSqr,
  phase,

  -- * Conjugate
  conjugate,

) where

import qualified Number.Complex                                     as Complex

import qualified Data.Array.Accelerate.Algebra.Absolute             as Absolute
import qualified Data.Array.Accelerate.Algebra.Additive             as Additive
import qualified Data.Array.Accelerate.Algebra.Algebraic            as Algebraic
import qualified Data.Array.Accelerate.Algebra.Field                as Field
import qualified Data.Array.Accelerate.Algebra.RealTranscendental   as RealTrans
import qualified Data.Array.Accelerate.Algebra.Ring                 as Ring
import qualified Data.Array.Accelerate.Algebra.Transcendental       as Trans
import qualified Data.Array.Accelerate.Algebra.ZeroTestable         as ZeroTestable

import Data.Array.Accelerate.Algebra.Absolute                       hiding ( C, signum )
import Data.Array.Accelerate.Algebra.Additive                       hiding ( C )
import Data.Array.Accelerate.Algebra.Algebraic                      hiding ( C )
import Data.Array.Accelerate.Algebra.Field                          hiding ( C )
import Data.Array.Accelerate.Algebra.RealTranscendental             hiding ( C )
import Data.Array.Accelerate.Algebra.Ring                           hiding ( C )
import Data.Array.Accelerate.Algebra.Transcendental                 hiding ( C, exp )
import Data.Array.Accelerate.Algebra.ZeroTestable                   hiding ( C )

import Data.Array.Accelerate                                        ( Lift(..), Unlift(..), (&&*), (<*), lift1, lift2, ifThenElse )
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar
import qualified Data.Array.Accelerate                              as A

import Prelude                                                      ( ($), (.), undefined )

#if __GLASGOW_HASKELL__ <= 708
import Data.Typeable
#endif


infix 6 +:

-- Cartesian form
-- --------------

uncons :: Elt a => Exp (Complex.T a) -> (Exp a, Exp a)
uncons z = (real z, imag z)

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
exp :: forall a. (Trans.C (Exp a), Elt a) => Exp (Complex.T a) -> Exp (Complex.T a)
exp = lift1 (Complex.exp :: Complex.T (Exp a) -> Complex.T (Exp a))

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
toPolar :: (RealTrans.C (Exp a), ZeroTestable.C (Exp a), Elt a) => Exp (Complex.T a) -> Exp (a, a)
toPolar z = lift (magnitude z, phase z)

-- | Form a complex number from polar components of magnitude and phase.
--
fromPolar :: (Trans.C (Exp a), Elt a) => Exp a -> Exp a -> Exp (Complex.T a)
fromPolar r theta = scale r (cis theta)

-- | @'cis' t@ is a complex value with magnitude @1@ and phase @t@ (modulo
-- @2*'pi'@).
--
cis :: forall a. (Trans.C (Exp a), Elt a) => Exp a -> Exp (Complex.T a)
cis = lift1 (Complex.cis :: Exp a -> Complex.T (Exp a))

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
magnitude :: (Algebraic.C (Exp a), Elt a) => Exp (Complex.T a) -> Exp a
magnitude = sqrt . magnitudeSqr

-- | Like NormedEuc.normSqr with lifted class constraints
--
magnitudeSqr :: forall a. (Ring.C (Exp a), Elt a) => Exp (Complex.T a) -> Exp a
magnitudeSqr = lift1 (Complex.magnitudeSqr :: Complex.T (Exp a) -> Exp a)

-- | The phase of a complex number, in the range @(-'pi', 'pi']@. If the
-- magnitude is zero, then so is the phase.
--
phase :: (RealTrans.C (Exp a), ZeroTestable.C (Exp a), Elt a) => Exp (Complex.T a) -> Exp a
phase z =
  if isZero z
     then zero
     else atan2 (imag z) (real z)

-- | Scale a complex number to magnitude 1.
--
-- For a complex number @z@, @'abs' z@ is a number with the magnitude of @z@,
-- but oriented in the positive real direction, whereas @'signum' z@ has the
-- phase of @z@, but unit magnitude.
--
signum :: (Algebraic.C (Exp a), ZeroTestable.C (Exp a), Elt a) => Exp (Complex.T a) -> Exp (Complex.T a)
signum z =
   if isZero z
     then zero
     else scale (recip (magnitude z)) z

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


-- numeric-prelude instances
-- -------------------------

instance (Additive.C (Exp a), Elt a) => Additive.C (Exp (Complex.T a)) where
  zero   = lift (zero :: Complex.T (Exp a))
  (+)    = lift2 ((+) :: Complex.T (Exp a) -> Complex.T (Exp a) -> Complex.T (Exp a))
  (-)    = lift2 ((-) :: Complex.T (Exp a) -> Complex.T (Exp a) -> Complex.T (Exp a))
  negate = lift1 (negate :: Complex.T (Exp a) -> Complex.T (Exp a))

instance (Ring.C (Exp a), Elt a) => Ring.C (Exp (Complex.T a)) where
  (*)           = lift2 ((*) :: Complex.T (Exp a) -> Complex.T (Exp a) -> Complex.T (Exp a))
  one           = lift (one :: Complex.T (Exp a))
  fromInteger x = lift (fromInteger x :: Complex.T (Exp a))

instance (Field.C (Exp a), Elt a) => Field.C (Exp (Complex.T a)) where
  (/)             = lift2 ((/) :: Complex.T (Exp a) -> Complex.T (Exp a) -> Complex.T (Exp a))
  fromRational' x = lift (fromRational' x :: Complex.T (Exp a))

instance (Absolute.C (Exp a), Algebraic.C (Exp a), ZeroTestable.C (Exp a), Elt a) => Absolute.C (Exp (Complex.T a)) where
  abs x  = magnitude x +: zero
  signum = signum

instance (ZeroTestable.C (Exp a), Elt a) => ZeroTestable.C (Exp (Complex.T a)) where
  isZero c = isZero (real c)
         &&* isZero (imag c)

instance (Absolute.C (Exp a), Algebraic.C (Exp a), Field.C (Exp a), RealTrans.C (Exp a), Trans.C (Exp a), ZeroTestable.C (Exp a), A.Ord a, Elt a)
    => Algebraic.C (Exp (Complex.T a)) where
  sqrt z@(uncons -> (x,y)) =
    if isZero z
       then zero
       else let
                u'  = sqrt ((magnitude z + abs x) / 2)
                v'  = abs y / (u' * 2)
                u   = if x <* zero then v' else u'
                v   = if x <* zero then u' else v'
            in
            u +: if y <* zero then -v else v
  --
  x ^/ r =
    let (mag, arg) = unlift (toPolar x)
    in  fromPolar (mag ^/ r) (arg * fromRational' r)

instance (Field.C (Exp a), RealTrans.C (Exp a), Trans.C (Exp a), ZeroTestable.C (Exp a), A.Ord a, Elt a)
    => Trans.C (Exp (Complex.T a)) where
  pi    = pi +: zero
  exp   = exp
  log z = let (m,p) = unlift (toPolar z)
          in  log m +: p
  sin  (uncons -> (x,y))   = (sin x * cosh y) +: ( cos x * sinh y)
  cos  (uncons -> (x,y))   = (cos x * cosh y) +: (-sin x * sinh y)
  sinh (uncons -> (x,y))   = (cos y * sinh x) +: ( sin y * cosh x)
  cosh (uncons -> (x,y))   = (cos y * cosh x) +: ( sin y * sinh x)
  asin z                   = quarterRight (log (quarterLeft z + sqrt (1 - z^2)))
  acos z                   = quarterRight (log (z + quarterLeft (sqrt (1 - z^2))))
  atan z@(uncons -> (x,y)) = quarterRight (log (((1-y) +: x) / sqrt (1+z^2)))


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

