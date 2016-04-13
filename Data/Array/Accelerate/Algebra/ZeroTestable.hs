{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Algebra.ZeroTestable
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Algebra.ZeroTestable (

  C(..),
  defltIsZero,

) where

import qualified Data.Array.Accelerate.Algebra.Additive   as Additive

import Data.Array.Accelerate                              as A
import qualified Prelude                                  as P


class C a where
  isZero :: Exp a -> Exp Bool         -- operand really doesn't need to be Exp

instance C Float  where isZero = defltIsZero
instance C Double where isZero = defltIsZero

instance C Int    where isZero = defltIsZero
instance C Int8   where isZero = defltIsZero
instance C Int16  where isZero = defltIsZero
instance C Int32  where isZero = defltIsZero
instance C Int64  where isZero = defltIsZero

instance C Word   where isZero = defltIsZero
instance C Word8  where isZero = defltIsZero
instance C Word16 where isZero = defltIsZero
instance C Word32 where isZero = defltIsZero
instance C Word64 where isZero = defltIsZero

instance C Z where
  isZero _ = constant P.True

instance (C sh, Slice sh) => C (sh :. Int) where
  isZero sh = isZero (indexHead sh) &&* isZero (indexTail sh)

instance (C a, C b, Elt a, Elt b) => C (a, b) where
  isZero x = let (a,b) = unlift x in isZero a &&* isZero b

instance (C a, C b, C c, Elt a, Elt b, Elt c) => C (a, b, c) where
  isZero x = let (a,b,c) = unlift x in isZero a &&* isZero b &&* isZero c

instance (C a, C b, C c, C d, Elt a, Elt b, Elt c, Elt d) => C (a, b, c, d) where
  isZero x = let (a,b,c,d) = unlift x in isZero a &&* isZero b &&* isZero c &&* isZero d

instance (C a, C b, C c, C d, C e, Elt a, Elt b, Elt c, Elt d, Elt e) => C (a, b, c, d, e) where
  isZero x = let (a,b,c,d,e) = unlift x in isZero a &&* isZero b &&* isZero c &&* isZero d &&* isZero e

instance (C a, C b, C c, C d, C e, C f, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f) => C (a, b, c, d, e, f) where
  isZero x = let (a,b,c,d,e,f) = unlift x in isZero a &&* isZero b &&* isZero c &&* isZero d &&* isZero e &&* isZero f

instance (C a, C b, C c, C d, C e, C f, C g, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g) => C (a, b, c, d, e, f, g) where
  isZero x = let (a,b,c,d,e,f,g) = unlift x in isZero a &&* isZero b &&* isZero c &&* isZero d &&* isZero e &&* isZero f &&* isZero g

instance (C a, C b, C c, C d, C e, C f, C g, C h, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h) => C (a, b, c, d, e, f, g, h) where
  isZero x = let (a,b,c,d,e,f,g,h) = unlift x in isZero a &&* isZero b &&* isZero c &&* isZero d &&* isZero e &&* isZero f &&* isZero g &&* isZero h

instance (C a, C b, C c, C d, C e, C f, C g, C h, C i, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i) => C (a, b, c, d, e, f, g, h, i) where
  isZero x = let (a,b,c,d,e,f,g,h,i) = unlift x in isZero a &&* isZero b &&* isZero c &&* isZero d &&* isZero e &&* isZero f &&* isZero g &&* isZero h &&* isZero i


-- | Checks if a number is the zero element. This test is not possible for all
-- 'Additive.C' types, since e.g. a function type does not belong to Eq. isZero
-- is possible for some types where (==zero) fails because there is no unique
-- zero.
--
-- Examples are vector (the length of the zero vector is unknown), physical
-- values (the unit of a zero quantity is unknown), residue class (the modulus
-- is unknown).
--
defltIsZero :: (Additive.C a, IsNum a, Elt a) => Exp a -> Exp Bool
defltIsZero = (Additive.zero ==*)

