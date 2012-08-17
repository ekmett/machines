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
-- Portability :  rank-2
--
----------------------------------------------------------------------------
module Data.Machine.Tee
  (
  -- * Tees
    Tee, TeeT
  , T(..)
  , tee
  , addL, addR
  , capL, capR
  ) where

import Control.Category
import Data.Machine.Process
import Data.Machine.Type
import Data.Machine.Source
import Prelude hiding ((.),id)

-------------------------------------------------------------------------------
-- Tees
-------------------------------------------------------------------------------

-- | The input descriptor for a 'Tee' or 'TeeT'
data T i c where
  L :: (a -> c) -> T (Either a b) c
  R :: (b -> c) -> T (Either a b) c

instance Functor (T i) where
  fmap f (L k) = L (f . k)
  fmap f (R k) = R (f . k)

-- | A 'Machine' that can read from two input stream in a deterministic manner.
type Tee a b c = Machine T (Either a b) c

-- | A 'Machine' that can read from two input stream in a deterministic manner with monadic side-effects.
type TeeT m a b c = MachineT m T (Either a b) c

-- | Compose a pair of pipes onto the front of a Tee.
tee :: Monad m => ProcessT m a a' -> ProcessT m b b' -> TeeT m a' b' c -> TeeT m a b c
tee ma mb m = MachineT $ runMachineT m >>= \v -> case v of
  Stop              -> return Stop
  Yield o k         -> return $ Yield o $ tee ma mb k
  Await f (L kf) ff -> runMachineT ma >>= \u -> case u of
    Stop            -> runMachineT $ tee stopped mb ff
    Yield a k       -> runMachineT $ tee k mb $ f $ kf a
    Await g kg fg ->
      return $ Await id (L (\a -> tee (g $ kg a) mb $ encased v)) $ tee fg mb $ encased v
  Await f (R kf) ff -> runMachineT mb >>= \u -> case u of
    Stop            -> runMachineT $ tee ma stopped ff
    Yield b k       -> runMachineT $ tee ma k $ f $ kf b
    Await g kg fg ->
      return $ Await id (R (\b -> tee ma (g $ kg b) $ encased v)) $ tee ma fg $ encased v

-- | Precompose a pipe onto the left input of a tee.
addL :: Monad m => ProcessT m a b -> TeeT m b c d -> TeeT m a c d
addL p = tee p id

-- | Precompose a pipe onto the right input of a tee.
addR :: Monad m => ProcessT m b c -> TeeT m a c d -> TeeT m a b d
addR = tee id

-- | Tie off one input of a tee by connecting it to a known source.
capL :: Monad m => SourceT m a -> TeeT m a b c -> ProcessT m b c
capL s t = fitting cappedT $ addL s t

-- | Tie off one input of a tee by connecting it to a known source.
capR :: Monad m => SourceT m b -> TeeT m a b c -> ProcessT m a c
capR s t = fitting cappedT $ addR s t

-- | Natural transformation used by 'capL' and 'capR'.
cappedT :: T (Either a a) b -> a -> b
cappedT (R r) = r
cappedT (L r) = r
