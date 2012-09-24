{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Plan
-- Copyright   :  (C) 2012 Edward Kmett, Rúnar Bjarnason
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank-N Types, MPTCs
--
----------------------------------------------------------------------------
module Data.Machine.Plan
  (
  -- * Plans
    Plan
  , runPlan
  , PlanT(..)
  , yield
  , await
  , stop
  , awaits
  ) where

import Control.Applicative
import Control.Category
import Control.Monad (ap, MonadPlus(..))
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad.Error.Class
import Control.Monad.Writer.Class
import Data.Functor.Identity
import Prelude hiding ((.),id)

-------------------------------------------------------------------------------
-- Plans
-------------------------------------------------------------------------------

-- | You can 'construct' a 'Plan' (or 'PlanT'), turning it into a
-- 'Data.Machine.Type.Machine' (or 'Data.Machine.Type.MachineT').
--
newtype PlanT k o m a = PlanT
  { runPlanT :: forall r.
      (a -> m r) ->                                     -- Done a
      (o -> m r -> m r) ->                              -- Yield o (Plan k o a)
      (forall z. (z -> m r) -> k z -> m r -> m r) ->  -- forall z. Await (z -> Plan o a) (k z) (Plan k o a)
      m r ->                                            -- Fail
      m r
  }

-- | A @'Plan' k o a@ is a specification for a pure 'Machine', that reads inputs selected by @k@
-- with types based on @i@, writes values of type @o@, and has intermediate results of type @a@.
--
-- A @'PlanT' k o a@ can be used as a @'PlanT' k o m a@ for any @'Monad' m@.
--
-- It is perhaps easier to think of 'Plan' in its un-cps'ed form, which would
-- look like:
--
-- @
-- data 'Plan' k o a
--   = Done a
--   | Yield o (Plan k o a)
--   | forall z. Await (z -> Plan k o a) (k z) (Plan k o a)
--   | Fail
-- @
type Plan k o a = forall m. PlanT k o m a

-- | Deconstruct a 'Plan' without reference to a 'Monad'.
runPlan :: PlanT k o Identity a
        -> (a -> r)
        -> (o -> r -> r)
        -> (forall z. (z -> r) -> k z -> r -> r)
        -> r
        -> r
runPlan m kp ke kr kf = runIdentity $ runPlanT m
  (Identity . kp)
  (\o (Identity r) -> Identity (ke o r))
  (\f k (Identity r) -> Identity (kr (runIdentity . f) k r))
  (Identity kf)

instance Functor (PlanT k o m) where
  fmap f (PlanT m) = PlanT $ \k -> m (k . f)

instance Applicative (PlanT k o m) where
  pure a = PlanT (\kp _ _ _ -> kp a)
  (<*>) = ap

instance Alternative (PlanT k o m) where
  empty = PlanT $ \_ _ _ kf -> kf
  PlanT m <|> PlanT n = PlanT $ \kp ke kr kf -> m kp ke (\ks kir _ -> kr ks kir (n kp ke kr kf)) (n kp ke kr kf)

instance Monad (PlanT k o m) where
  return a = PlanT (\kp _ _ _ -> kp a)
  PlanT m >>= f = PlanT (\kp ke kr kf -> m (\a -> runPlanT (f a) kp ke kr kf) ke kr kf)
  fail _ = PlanT (\_ _ _ kf -> kf)

instance MonadPlus (PlanT k o m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans (PlanT k o) where
  lift m = PlanT (\kp _ _ _ -> m >>= kp)

instance MonadIO m => MonadIO (PlanT k o m) where
  liftIO m = PlanT (\kp _ _ _ -> liftIO m >>= kp)

instance MonadState s m => MonadState s (PlanT k o m) where
  get = lift get
  put = lift . put
  state f = PlanT $ \kp _ _ _ -> state f >>= kp

instance MonadReader e m => MonadReader e (PlanT k o m) where
  ask = lift ask
  reader = lift . reader
  local f m = PlanT $ \kp ke kr kf -> local f (runPlanT m kp ke kr kf)

instance MonadWriter w m  => MonadWriter w (PlanT k o m) where
  writer = lift . writer
  tell   = lift . tell

  listen m = PlanT $ \kp ke kr kf -> runPlanT m ((kp =<<) . listen . return) ke kr kf

  pass m = PlanT $ \kp ke kr kf -> runPlanT m ((kp =<<) . pass . return) ke kr kf

instance MonadError e m => MonadError e (PlanT k o m) where
  throwError = lift . throwError
  catchError m k = PlanT $ \kp ke kr kf -> runPlanT m kp ke kr kf `catchError` \e -> runPlanT (k e) kp ke kr kf

-- | Output a result.
yield :: o -> Plan k o ()
yield o = PlanT (\kp ke _ _ -> ke o (kp ()))

-- | Wait for input.
--
-- @'await' = 'awaits' 'id'@
await :: Category k => Plan (k i) o i
await = PlanT (\kp _ kr kf -> kr kp id kf)

-- | Wait for a particular input.
--
-- @
-- awaits 'L'  :: 'Plan' ('T' a b) o a
-- awaits 'R'  :: 'Plan' ('T' a b) o b
-- awaits 'id' :: 'Plan' ('Data.Machine.Is.Is' i) o i
-- @
awaits :: k i -> Plan k o i
awaits h = PlanT $ \kp _ kr -> kr kp h

-- | @'stop' = 'empty'@
stop :: Plan k o a
stop = empty
