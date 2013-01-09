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
    Tee, TeeT
  , T(..)
  , tee
  , addL, addR
  , capL, capR
  ) where

import Data.Machine.Is
import Data.Machine.Process
import Data.Machine.Type
import Data.Machine.Source
import Prelude hiding ((.),id)

-------------------------------------------------------------------------------
-- Tees
-------------------------------------------------------------------------------

-- | The input descriptor for a 'Tee' or 'TeeT'
data T a b c where
  L :: T a b a
  R :: T a b b

-- | A 'Machine' that can read from two input stream in a deterministic manner.
type Tee a b c = Machine (T a b) c

-- | A 'Machine' that can read from two input stream in a deterministic manner with monadic side-effects.
type TeeT m a b c = MachineT m (T a b) c

-- | Compose a pair of pipes onto the front of a Tee.
tee :: Monad m => ProcessT m a a' -> ProcessT m b b' -> TeeT m a' b' c -> TeeT m a b c
tee ma mb m = MachineT $ runMachineT m >>= \v -> case v of
  Stop         -> return Stop
  Yield o k    -> return $ Yield o $ tee ma mb k
  Await f L ff -> runMachineT ma >>= \u -> case u of
    Stop            -> runMachineT $ tee stopped mb ff
    Yield a k       -> runMachineT $ tee k mb $ f a
    Await g Refl fg ->
      return $ Await (\a -> tee (g a) mb $ encased v) L $ tee fg mb $ encased v
  Await f R ff -> runMachineT mb >>= \u -> case u of
    Stop            -> runMachineT $ tee ma stopped ff
    Yield b k       -> runMachineT $ tee ma k $ f b
    Await g Refl fg ->
      return $ Await (\b -> tee ma (g b) $ encased v) R $ tee ma fg $ encased v

-- | Precompose a pipe onto the left input of a tee.
addL :: Monad m => ProcessT m a b -> TeeT m b c d -> TeeT m a c d
addL p = tee p echo
{-# INLINE addL #-}

-- | Precompose a pipe onto the right input of a tee.
addR :: Monad m => ProcessT m b c -> TeeT m a c d -> TeeT m a b d
addR = tee echo
{-# INLINE addR #-}

-- | Tie off one input of a tee by connecting it to a known source.
capL :: Monad m => SourceT m a -> TeeT m a b c -> ProcessT m b c
capL s t = fit cappedT $ addL s t
{-# INLINE capL #-}

-- | Tie off one input of a tee by connecting it to a known source.
capR :: Monad m => SourceT m b -> TeeT m a b c -> ProcessT m a c
capR s t = fit cappedT $ addR s t
{-# INLINE capR #-}

-- | Natural transformation used by 'capL' and 'capR'.
cappedT :: T a a b -> Is a b
cappedT R = Refl
cappedT L = Refl
{-# INLINE cappedT #-}
