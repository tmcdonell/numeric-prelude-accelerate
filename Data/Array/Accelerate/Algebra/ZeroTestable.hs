{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
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
import qualified Algebra.ZeroTestable                     as NP

import Data.Array.Accelerate                              as A
import Data.List                                          ( unlines )


class C a where
  isZero :: a -> Exp Bool

instance C (Exp Float)  where isZero = defltIsZero
instance C (Exp Double) where isZero = defltIsZero

instance C (Exp Int)    where isZero = defltIsZero
instance C (Exp Int8)   where isZero = defltIsZero
instance C (Exp Int16)  where isZero = defltIsZero
instance C (Exp Int32)  where isZero = defltIsZero
instance C (Exp Int64)  where isZero = defltIsZero

instance C (Exp Word)   where isZero = defltIsZero
instance C (Exp Word8)  where isZero = defltIsZero
instance C (Exp Word16) where isZero = defltIsZero
instance C (Exp Word32) where isZero = defltIsZero
instance C (Exp Word64) where isZero = defltIsZero

instance C (Exp Z) where
  isZero _ = constant True

instance (C (Exp sh), Slice sh) => C (Exp (sh :. Int)) where
  isZero sh = isZero (indexHead sh) && isZero (indexTail sh)

instance (C (Exp a), C (Exp b), Elt a, Elt b) => C (Exp (a, b)) where
  isZero x = let (a,b) = unlift x :: (Exp a, Exp b)
             in  isZero a && isZero b

instance (C (Exp a), C (Exp b), C (Exp c), Elt a, Elt b, Elt c) => C (Exp (a, b, c)) where
  isZero x = let (a,b,c) = unlift x :: (Exp a, Exp b, Exp c)
             in  isZero a && isZero b && isZero c

instance (C (Exp a), C (Exp b), C (Exp c), C (Exp d), Elt a, Elt b, Elt c, Elt d) => C (Exp (a, b, c, d)) where
  isZero x = let (a,b,c,d) = unlift x :: (Exp a, Exp b, Exp c, Exp d)
             in  isZero a && isZero b && isZero c && isZero d

instance (C (Exp a), C (Exp b), C (Exp c), C (Exp d), C (Exp e), Elt a, Elt b, Elt c, Elt d, Elt e) => C (Exp (a, b, c, d, e)) where
  isZero x = let (a,b,c,d,e) = unlift x :: (Exp a, Exp b, Exp c, Exp d, Exp e)
             in  isZero a && isZero b && isZero c && isZero d && isZero e

instance (C (Exp a), C (Exp b), C (Exp c), C (Exp d), C (Exp e), C (Exp f), Elt a, Elt b, Elt c, Elt d, Elt e, Elt f) => C (Exp (a, b, c, d, e, f)) where
  isZero x = let (a,b,c,d,e,f) = unlift x :: (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f)
             in  isZero a && isZero b && isZero c && isZero d && isZero e && isZero f

instance (C (Exp a), C (Exp b), C (Exp c), C (Exp d), C (Exp e), C (Exp f), C (Exp g), Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g) => C (Exp (a, b, c, d, e, f, g)) where
  isZero x = let (a,b,c,d,e,f,g) = unlift x :: (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g)
             in  isZero a && isZero b && isZero c && isZero d && isZero e && isZero f && isZero g

instance (C (Exp a), C (Exp b), C (Exp c), C (Exp d), C (Exp e), C (Exp f), C (Exp g), C (Exp h), Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h) => C (Exp (a, b, c, d, e, f, g, h)) where
  isZero x = let (a,b,c,d,e,f,g,h) = unlift x :: (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h)
             in  isZero a && isZero b && isZero c && isZero d && isZero e && isZero f && isZero g && isZero h

instance (C (Exp a), C (Exp b), C (Exp c), C (Exp d), C (Exp e), C (Exp f), C (Exp g), C (Exp h), C (Exp i), Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i) => C (Exp (a, b, c, d, e, f, g, h, i)) where
  isZero x = let (a,b,c,d,e,f,g,h,i) = unlift x :: (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i)
             in  isZero a && isZero b && isZero c && isZero d && isZero e && isZero f && isZero g && isZero h && isZero i


-- | Checks if a number is the zero element. This test is not possible for all
-- 'Additive.C' types, since e.g. a function type does not belong to Eq. isZero
-- is possible for some types where (==zero) fails because there is no unique
-- zero.
--
-- Examples are vector (the length of the zero vector is unknown), physical
-- values (the unit of a zero quantity is unknown), residue class (the modulus
-- is unknown).
--
defltIsZero :: (Additive.C (Exp a), A.Eq a, Elt a) => Exp a -> Exp Bool
defltIsZero = (Additive.zero ==)


-- Vacuous instances to satisfy NP superclass constraints
--

instance NP.C (Exp Float)  where isZero = npIsZeroError
instance NP.C (Exp Double) where isZero = npIsZeroError

instance NP.C (Exp Int)    where isZero = npIsZeroError
instance NP.C (Exp Int8)   where isZero = npIsZeroError
instance NP.C (Exp Int16)  where isZero = npIsZeroError
instance NP.C (Exp Int32)  where isZero = npIsZeroError
instance NP.C (Exp Int64)  where isZero = npIsZeroError

instance NP.C (Exp Word)   where isZero = npIsZeroError
instance NP.C (Exp Word8)  where isZero = npIsZeroError
instance NP.C (Exp Word16) where isZero = npIsZeroError
instance NP.C (Exp Word32) where isZero = npIsZeroError
instance NP.C (Exp Word64) where isZero = npIsZeroError

instance (NP.C (Exp a), NP.C (Exp b)) => NP.C (Exp (a,b)) where
  isZero = npIsZeroError

instance (NP.C (Exp a), NP.C (Exp b), NP.C (Exp c)) => NP.C (Exp (a,b,c)) where
  isZero = npIsZeroError


npIsZeroError :: a -> Bool
npIsZeroError
  = error
  $ unlines [ "Algebra.ZeroTestable.isZero is incompatible with Accelerate"
            , "use Data.Array.Accelerate.Algebra.ZeroTestable.isZero instead"
            ]

