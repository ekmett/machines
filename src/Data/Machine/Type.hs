{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Type
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  rank-2, GADTs
--
----------------------------------------------------------------------------
module Data.Machine.Type
  (
  -- * Machines
    MachineT(..)
  , Step(..)
  , Machine
  , run
  , runMachine

  -- ** Building machines from plans
  , construct
  , repeatedly
  , before
--  , sink

  -- * Reshaping machines
  , fitting
  , pass

  , stopped
  ) where

import Control.Applicative
import Control.Category
import Control.Monad (liftM)
import Data.Foldable
import Data.Machine.Id
import Data.Machine.Plan
import Data.Monoid
import Data.Profunctor
import Prelude hiding ((.),id)

-------------------------------------------------------------------------------
-- Transduction Machines
-------------------------------------------------------------------------------

-- | This is the base functor for a 'Machine' or 'MachineT'.
--
-- Note: Machines are usually constructed from 'Plan', so this does not need to be CPS'd.
data Step k i o r
  = Stop
  | Yield o r
  | forall t. Await (t -> r) (k i t) r

instance Functor (Step k i o) where
  fmap _ Stop = Stop
  fmap f (Yield o k) = Yield o (f k)
  fmap f (Await g kg fg) = Await (f . g) kg (f fg)

-- | A 'MachineT' reads from a number of inputs and may yield results before stopping
-- with monadic side-effects.
newtype MachineT m k i o = MachineT { runMachineT :: m (Step k i o (MachineT m k i o)) }

-- | A 'Machine' reads from a number of inputs and may yield results before stopping.
--
-- A 'Machine' can be used as a @'MachineT' m@ for any @'Monad' m@.
type Machine k i o = forall m. Monad m => MachineT m k i o

-- | @'runMachine' = 'runId' . 'runMachineT'@
runMachine :: MachineT Id k i o -> Step k i o (MachineT Id k i o)
runMachine = runId . runMachineT

instance Monad m => Functor (MachineT m k i) where
  fmap f (MachineT m) = MachineT (liftM f' m) where
    f' (Yield o xs)    = Yield (f o) (f <$> xs)
    f' (Await k kir e) = Await (fmap f . k) kir (f <$> e)
    f' Stop            = Stop

instance (Monad m, Profunctor k) => Profunctor (MachineT m k) where
  rmap = fmap
  lmap f (MachineT m) = MachineT (liftM f' m) where
    f' (Yield o xs)    = Yield o (lmap f xs)
    f' (Await k kir e) = Await (lmap f . k) (lmap f kir) (lmap f e)
    f' Stop            = Stop

-- | Stop feeding input into model and extract an answer
runT :: Monad m => MachineT m k a b -> m [b]
runT (MachineT m) = m >>= \v -> case v of
  Stop        -> return []
  Yield o k   -> liftM (o:) (runT k)
  Await _ _ e -> runT e

-- | Run a pure machine and extract an answer.
run :: MachineT Id k a b -> [b]
run = runId . runT

-- | This permits toList to be used on a Machine.
instance Foldable m => Foldable (MachineT m k i) where
  foldMap f (MachineT m) = foldMap go m where
    go Stop = mempty
    go (Yield o k) = f o `mappend` foldMap f k
    go (Await _ _ fg) = foldMap f fg

-- |
-- Connect different kinds of machines.
--
-- @'fitting' 'id' = 'id'@
--
-- @
-- 'fitting' 'Data.Machine.Tee.L' :: 'Data.Machine.Process.Process' a c -> 'Data.Machine.Tee.Tee' a b c
-- 'fitting' 'Data.Machine.Tee.R' :: 'Data.Machine.Process.Process' b c -> 'Data.Machine.Tee.Tee' a b c
-- 'fitting' 'id' :: 'Data.Machine.Process.Process' a b -> 'Data.Machine.Process.Process' a b
-- @
fitting :: Monad m => (forall a. k i a -> k' i' a) -> MachineT m k i o -> MachineT m k' i' o
fitting f (MachineT m) = MachineT (liftM f' m) where
  f' (Yield o k)     = Yield o (fitting f k)
  f' Stop            = Stop
  f' (Await g kir h) = Await (fitting f . g) (f kir) (fitting f h)

-- | Compile a machine to a model.
construct :: Monad m => PlanT k i o m a -> MachineT m k i o
construct m = MachineT $ runPlanT m
  (const (return Stop))
  (\o k -> return (Yield o (MachineT k)))
  (\f k g -> return (Await (MachineT . f) k (MachineT g)))
  (return Stop)

-- | Generates a model that runs a machine until it stops, then start it up again.
--
-- @'repeatedly' m = 'construct' ('forever' m)@
repeatedly :: Monad m => PlanT k i o m a -> MachineT m k i o
repeatedly m = r where
  r = MachineT $ runPlanT m
    (const (runMachineT r))
    (\o k -> return (Yield o (MachineT k)))
    (\f k g -> return (Await (MachineT . f) k (MachineT g)))
    (return Stop)

-- | Evaluate a machine until it stops, and then yield answers according to the supplied model.
before :: Monad m => MachineT m k i o -> PlanT k i o m a -> MachineT m k i o
before (MachineT n) m = MachineT $ runPlanT m
  (const n)
  (\o k -> return (Yield o (MachineT k)))
  (\f k g -> return (Await (MachineT . f) k (MachineT g)))
  (return Stop)

-- | Given a handle, ignore all other inputs and just stream input from that handle.
--
-- @
-- 'pass' 'id' :: 'Data.Machine.Process.Process' a a
-- 'pass' 'Data.Machine.Tee.L'  :: 'Data.Machine.Tee.Tee' a b a
-- 'pass' 'Data.Machine.Tee.R'  :: 'Data.Machine.Tee.Tee' a b b
-- @
pass :: Functor (k i) => Handle k i o -> Machine k i o
pass input = repeatedly $ do
  a <- awaits input
  yield a

instance Monad m => Category (MachineT m (->)) where
  id = repeatedly $ do
    i <- await
    yield i

  m . n = MachineT $ runMachineT m >>= \v -> case v of
    Stop          -> return Stop
    Yield a as    -> return $ Yield a (as . n)
    Await f kir k -> runMachineT n >>= \u -> case u of
      Stop          -> runMachineT (k . MachineT (return Stop))
      Yield b bs    -> runMachineT (fmap f kir b . bs)
      Await g kg fg -> let mv = MachineT (return v) in
        return (Await (\a -> mv . g a) kg (mv . fg))

-- | This is a stopped 'Machine'
stopped :: Machine k a b
stopped = MachineT (return Stop)

-------------------------------------------------------------------------------
-- Sink
-------------------------------------------------------------------------------

{-
-- |
-- A Sink in this model is a 'Data.Machine.Process.Process'
-- (or 'Data.Machine.Tee.Tee', etc) that produces a single answer.
--
-- \"Is that your final answer?\"
sink :: Monad m => (forall o. PlanT k i o m a) -> MachineT m k i a
sink m = runPlanT m (\a -> Yield a Stop) id (Await id) Stop
-}
