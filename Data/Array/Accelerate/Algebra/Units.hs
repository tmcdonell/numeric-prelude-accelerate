{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Algebra.Units
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Algebra.Units (

  C(..)

) where

import qualified Data.Array.Accelerate.Algebra.Absolute             as Absolute
import qualified Data.Array.Accelerate.Algebra.IntegralDomain       as Integral
import qualified Data.Array.Accelerate.Algebra.Ring                 as Ring
import qualified Data.Array.Accelerate.Algebra.ZeroTestable         as ZeroTestable

import Data.Array.Accelerate.Algebra.Absolute                       ( abs, signum )
import Data.Array.Accelerate.Algebra.Additive                       ( negate )
import Data.Array.Accelerate.Algebra.IntegralDomain                 ( div )
import Data.Array.Accelerate.Algebra.Ring                           ( one, (*) )
import Data.Array.Accelerate.Algebra.ZeroTestable                   ( isZero )

import qualified Algebra.Units                                      as NP

import Data.Array.Accelerate                                        as A ( Exp, Elt, Eq(..), (||), ifThenElse )
import Data.Array.Accelerate.Type                                   as A
import Prelude                                                      as P ( ($), error, unlines )


-- | This class lets us deal with units in a ring. 'isUnit' tells us whether an
-- element is a unit, while the other operations let us canonically write an
-- element as a unit times another element.
--
-- This instance shadows 'Algebra.Units.C' with an instance of 'isUnit'
-- compatible with Accelerate.
--
class Integral.C a => C a where
  {-# MINIMAL isUnit, (stdUnit | stdUnitInv) #-}
  isUnit :: a -> Exp Bool

  stdAssociate :: a -> a
  stdAssociate x = x * stdUnitInv x

  stdUnit :: a -> a
  stdUnit      x = one `div` stdUnitInv x  -- should be divChecked

  stdUnitInv :: a -> a
  stdUnitInv   x = one `div` stdUnit x


instance C (Exp Int) where
  isUnit       = defltIsUnit
  stdAssociate = defltStdAssociate
  stdUnit      = defltStdUnit
  stdUnitInv   = defltStdUnitInv

instance C (Exp Int8) where
  isUnit       = defltIsUnit
  stdAssociate = defltStdAssociate
  stdUnit      = defltStdUnit
  stdUnitInv   = defltStdUnitInv

instance C (Exp Int16) where
  isUnit       = defltIsUnit
  stdAssociate = defltStdAssociate
  stdUnit      = defltStdUnit
  stdUnitInv   = defltStdUnitInv

instance C (Exp Int32) where
  isUnit       = defltIsUnit
  stdAssociate = defltStdAssociate
  stdUnit      = defltStdUnit
  stdUnitInv   = defltStdUnitInv

instance C (Exp Int64) where
  isUnit       = defltIsUnit
  stdAssociate = defltStdAssociate
  stdUnit      = defltStdUnit
  stdUnitInv   = defltStdUnitInv


-- numeric-prelude instances for all operations aside from 'isUnit'.
--
instance NP.C (Exp Int) where
  isUnit       = npIsUnitError
  stdAssociate = defltStdAssociate
  stdUnit      = defltStdUnit
  stdUnitInv   = defltStdUnitInv

instance NP.C (Exp Int8) where
  isUnit       = npIsUnitError
  stdAssociate = defltStdAssociate
  stdUnit      = defltStdUnit
  stdUnitInv   = defltStdUnitInv

instance NP.C (Exp Int16) where
  isUnit       = npIsUnitError
  stdAssociate = defltStdAssociate
  stdUnit      = defltStdUnit
  stdUnitInv   = defltStdUnitInv

instance NP.C (Exp Int32) where
  isUnit       = npIsUnitError
  stdAssociate = defltStdAssociate
  stdUnit      = defltStdUnit
  stdUnitInv   = defltStdUnitInv

instance NP.C (Exp Int64) where
  isUnit       = npIsUnitError
  stdAssociate = defltStdAssociate
  stdUnit      = defltStdUnit
  stdUnitInv   = defltStdUnitInv


-- Default instance for atomic types
--
defltIsUnit :: (A.Eq a, Ring.C (Exp a)) => Exp a -> Exp Bool
defltIsUnit x = x == one || x == negate one

defltStdAssociate :: Absolute.C (Exp a) => Exp a -> Exp a
defltStdAssociate = abs

defltStdUnit :: (Absolute.C (Exp a), ZeroTestable.C (Exp a), Elt a) => Exp a -> Exp a
defltStdUnit x =
  if isZero x
     then x
     else signum x

defltStdUnitInv :: (Absolute.C (Exp a), ZeroTestable.C (Exp a), Elt a) => Exp a -> Exp a
defltStdUnitInv = defltStdUnit

npIsUnitError :: a -> Bool
npIsUnitError
  = error
  $ unlines [ "Algebra.Units.isUnit is incompatible with Accelerate"
            , "use Data.Array.Accelerate.Algebra.Units.isUnit instead"
            ]

