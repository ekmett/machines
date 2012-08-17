{-# LANGUAGE Rank2Types, GADTs #-}
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
  , Merge(..)
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
data Merge i c where
  L :: (a -> c) -> Merge (a, b) c
  R :: (b -> c) -> Merge (a, b) c

instance Functor (Merge i) where
  fmap f (L k) = L (f . k)
  fmap f (R k) = R (f . k)

-- | A 'Machine' that can read from two input stream in a deterministic manner.
type Tee a b c = Machine Merge (a, b) c

-- | A 'Machine' that can read from two input stream in a deterministic manner with monadic side-effects.
type TeeT m a b c = MachineT m Merge (a,b) c

-- | Compose a pair of pipes onto the front of a Tee.
tee :: Monad m => ProcessT m a a' -> ProcessT m b b' -> TeeT m a' b' c -> TeeT m a b c
tee ma mb m = MachineT $ runMachineT m >>= \v -> case v of
  Stop              -> return Stop
  Yield o k         -> return $ Yield o (tee ma mb k)
  Await f (L kf) ff -> runMachineT ma >>= \u -> case u of
    Stop            -> runMachineT $ tee stopped mb ff
    Yield a k       -> runMachineT $ tee k mb (fmap f kf a)
    Await g kg fg   -> let fm = MachineT (return (Await f (L kf) ff)) in
      return $ Await id (L (\a -> tee (fmap g kg a) mb fm)) (tee fg mb fm)
  Await f (R kf) ff -> runMachineT mb >>= \u -> case u of
    Stop            -> runMachineT $ tee ma stopped ff
    Yield b k       -> runMachineT $ tee ma k (fmap f kf b)
    Await g kg fg   -> let fm = MachineT (return (Await f (R kf) ff)) in
      return $ Await id (R (\b -> tee ma (fmap g kg b) fm)) (tee ma fg fm)

-- | Precompose a pipe onto the left input of a tee.
addL :: Monad m => ProcessT m a b -> TeeT m b c d -> TeeT m a c d
addL p = tee p id

-- | Precompose a pipe onto the right input of a tee.
addR :: Monad m => ProcessT m b c -> TeeT m a c d -> TeeT m a b d
addR = tee id

-- | Tie off one input of a tee by connecting it to a known source.
capL :: Monad m => SourceT m a -> TeeT m a b c -> ProcessT m b c
capL s t = fitting capped (addL s t)

-- | Tie off one input of a tee by connecting it to a known source.
capR :: Monad m => SourceT m b -> TeeT m a b c -> ProcessT m a c
capR s t = fitting capped (addR s t)

-- | Natural transformation used by 'capL' and 'capR'.
capped :: Merge (a, a) b -> a -> b
capped (R r) = r
capped (L r) = r

