{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Tee
-- Copyright   :  (C) 2012 Edward Kmett, Rúnar Bjarnason, Paul Chiusano
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank-2 Types, GADTs
--
----------------------------------------------------------------------------
module Data.Machine.Tee
  ( -- * Tees
    Tee
  , T(..)
  , tee
  , addL, addR
  , capL, capR
  ) where

import Control.Applicative
import Data.Machine.Process
import Data.Machine.Type
import Data.Machine.Source

-------------------------------------------------------------------------------
-- Tees
-------------------------------------------------------------------------------

-- | The input descriptor for a 'Tee' or 'TeeT'
data T a b c where
  L :: T a b a
  R :: T a b b

-- | A 'Machine' that can read from two input stream in a deterministic manner.
type Tee a b c = Machine (T a b) c

-- | Compose a pair of pipes onto the front of a Tee.
tee :: Process a a' -> Process b b' -> Tee a' b' c -> Tee a b c
tee ma mb m = case m of
  Stop         -> Stop
  Yield o k    -> Yield o $ tee ma mb k
  Await f L ff -> case ma of
    Stop            -> tee empty mb ff
    Yield a k       -> tee k mb $ f a
    Await g h fg -> Await (\a -> tee (g (h a)) mb m) L (tee fg mb m)
  Await f R ff -> case mb of
    Stop            -> tee ma empty ff
    Yield b k       -> tee ma k (f b)
    Await g h fg -> Await (\b -> tee ma (g (h b)) m) R (tee ma fg m)

-- | Precompose a pipe onto the left input of a tee.
addL :: Process a b -> Tee b c d -> Tee a c d
addL p = tee p echo
{-# INLINE addL #-}

-- | Precompose a pipe onto the right input of a tee.
addR :: Process b c -> Tee a c d -> Tee a b d
addR = tee echo
{-# INLINE addR #-}

-- | Tie off one input of a tee by connecting it to a known source.
capL :: Source a -> Tee a b c -> Process b c
capL s t = fit cappedT $ addL s t
{-# INLINE capL #-}

-- | Tie off one input of a tee by connecting it to a known source.
capR :: Source b -> Tee a b c -> Process a c
capR s t = fit cappedT $ addR s t
{-# INLINE capR #-}

-- | Natural transformation used by 'capL' and 'capR'.
cappedT :: T a a b -> a -> b
cappedT R = id
cappedT L = id
{-# INLINE cappedT #-}
