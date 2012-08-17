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
-- Portability :  rank-2, MPTCs
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
  -- * Handling multiple inputs
  , Handle
  , Fitting
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
import Data.Machine.Id
import Prelude hiding ((.),id)

-------------------------------------------------------------------------------
-- Plans
-------------------------------------------------------------------------------

-- | You can 'construct' a 'Plan' (or 'PlanT'), turning it into a
-- 'Data.Machine.Type.Machine' (or 'Data.Machine.Type.MachineT').
--
newtype PlanT k i o m a = PlanT
  { runPlanT :: forall r.
      (a -> m r) ->                                     -- Done a
      (o -> m r -> m r) ->                              -- Yield o (Plan k i o a)
      (forall z. (z -> m r) -> k i z -> m r -> m r) ->  -- forall z. Await (z -> Plan i o a) (k i z) (Plan k i o a)
      m r ->                                            -- Fail
      m r
  }


-- | A @'Plan' k i o a@ is a specification for a pure 'Machine', that reads inputs selected by @k@
-- with types based on @i@, writes values of type @o@, and has intermediate results of type @a@.
--
-- A @'PlanT' k i o a@ can be used as a @'PlanT' k i o m a@ for any @'Monad' m@.
--
-- It is perhaps easier to think of 'Plan' in its un-cps'ed form, which would
-- look like:
--
-- @
-- data 'Plan' k i o a
--   = Done a
--   | Yield o (Plan k i o a)
--   | forall z. Await (z -> Plan k i o a) (k i z) (Plan k i o a)
--   | Fail
-- @
type Plan k i o a = forall m. PlanT k i o m a

-- | Deconstruct a 'Plan' without reference to a 'Monad'.
runPlan :: PlanT k i o Id a
        -> (a -> r)
        -> (o -> r -> r)
        -> (forall z. (z -> r) -> k i z -> r -> r)
        -> r
        -> r
runPlan m kp ke kr kf = runId $ runPlanT m
  (Id . kp)
  (\o (Id r) -> Id (ke o r))
  (\f k (Id r) -> Id (kr (runId . f) k r))
  (Id kf)

instance Functor (PlanT k i o m) where
  fmap f (PlanT m) = PlanT $ \k -> m (k . f)

instance Applicative (PlanT k i o m) where
  pure a = PlanT (\kp _ _ _ -> kp a)
  (<*>) = ap

instance Alternative (PlanT k i o m) where
  empty = PlanT $ \_ _ _ kf -> kf
  PlanT m <|> PlanT n = PlanT $ \kp ke kr kf -> m kp ke (\ks kir _ -> kr ks kir (n kp ke kr kf)) (n kp ke kr kf)

instance Monad (PlanT k i o m) where
  return a = PlanT (\kp _ _ _ -> kp a)
  PlanT m >>= f = PlanT (\kp ke kr kf -> m (\a -> runPlanT (f a) kp ke kr kf) ke kr kf)
  fail _ = PlanT (\_ _ _ kf -> kf)

instance MonadPlus (PlanT k i o m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans (PlanT k i o) where
  lift m = PlanT (\kp _ _ _ -> m >>= kp)

instance MonadIO m => MonadIO (PlanT k i o m) where
  liftIO m = PlanT (\kp _ _ _ -> liftIO m >>= kp)

instance MonadState s m => MonadState s (PlanT k i o m) where
  get = lift get
  put = lift . put
  state f = PlanT $ \kp _ _ _ -> state f >>= kp

instance MonadReader e m => MonadReader e (PlanT k i o m) where
  ask = lift ask
  reader = lift . reader
  local f m = PlanT $ \kp ke kr kf -> local f (runPlanT m kp ke kr kf)

instance MonadError e m => MonadError e (PlanT k i o m) where
  throwError = lift . throwError
  catchError m k = PlanT $ \kp ke kr kf -> runPlanT m kp ke kr kf `catchError` \e -> runPlanT (k e) kp ke kr kf

-- | Output a result.
yield :: o -> Plan k i o ()
yield o = PlanT (\kp ke _ _ -> ke o (kp ()))

-- | Wait for input.
--
-- @'await' = 'awaits' 'id'@
await :: Plan (->) i o i
await = PlanT (\kp _ kr kf -> kr kp id kf)

-- | Many combinators are parameterized on the choice of 'Handle',
-- this acts like an input stream selector.
--
-- @
-- 'L' :: 'Handle' 'Merge' (a,b) a
-- 'R' :: 'Handle' 'Merge' (a,b) b
-- @
type Handle k i o = forall r. (o -> r) -> k i r

-- |
-- @type 'Handle' = 'Fitting' (->)@
type Fitting k k' o i = forall r. k o r -> k' i r

-- | Wait for a particular input.
--
-- @
-- awaits 'L'  :: 'Plan' 'Merge' (a,b) o a
-- awaits 'R'  :: 'Plan' 'Merge' (a,b) o b
-- awaits 'id' :: 'Plan' (->) i o i
-- @
awaits :: Handle k i j -> Plan k i o j
awaits f = PlanT $ \kp _ kr -> kr kp (f id)

-- | @'stop' = 'empty'@
stop :: Plan k i o a
stop = empty


