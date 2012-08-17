{-# LANGUAGE Rank2Types, GADTs, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Plan
-- Copyright   :  (C) 2012 Edward Kmett, Runar Bjarnason, Paul Chiusano
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
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
import Data.Functor.Identity
import Prelude hiding ((.),id)

-------------------------------------------------------------------------------
-- Plans
-------------------------------------------------------------------------------

-- | You can 'construct' a 'Plan', turning it into a 'Machine'
--
-- It is perhaps easier to think of 'Plan' in its un-cps'ed form, which would
-- look like:
--
-- @
-- data Plan k i o a
--   = Done a
--   | Yield o (Plan k i o a)
--   | Await (k i (Plan k i o a)) (Plan k i o a)
--   | Fail
-- @
newtype PlanT k i o m a = PlanT
  { runPlanT :: forall r.
      (a -> m r) ->                 -- Done a
      (o -> m r -> m r) ->          -- Yield o (Plan k i o a)
      (k i (m r) -> m r -> m r) ->  -- Await (k i (Plan k i o a)) (Plan k i o a)
      m r ->                        -- Fail
      m r
  }

type Plan k i o = PlanT k i o Identity

runPlan :: Functor (k i) => Plan k i o a -> (a -> r) -> (o -> r -> r) -> (k i r -> r -> r) -> r -> r
runPlan m ar orr kirrr z = runIdentity $ runPlanT m
  (Identity . ar)
  (\o (Identity r) -> Identity (orr o r))
  (\kimr (Identity r) -> Identity (kirrr (runIdentity <$> kimr) r))
  (Identity z)

instance Functor (PlanT k i o m) where
  fmap f (PlanT m) = PlanT $ \k -> m (k . f)

instance Applicative (PlanT k i o m) where
  pure a = PlanT (\kp _ _ _ -> kp a)
  (<*>) = ap

instance Alternative (PlanT k i o m) where
  empty = PlanT $ \_ _ _ kf -> kf
  PlanT m <|> PlanT n = PlanT $ \kp ke kr kf -> m kp ke (\kir _ -> kr kir (n kp ke kr kf)) kf

instance Monad (PlanT k i o m) where
  return a = PlanT (\kp _ _ _ -> kp a)
  PlanT m >>= f = PlanT (\kp ke kr kf -> m (\a -> runPlanT (f a) kp ke kr kf) ke kr kf)
  fail _ = PlanT (\_ _ _ kf -> kf)

instance MonadPlus (PlanT k i o m) where
  mzero = empty
  mplus = (<|>)

-- | Output a result.
yield :: o -> PlanT k i o m ()
yield o = PlanT (\kp ke _ _ -> ke o (kp ()))

-- | Wait for input.
--
-- @'await' = 'awaits' 'id'@
await :: PlanT (->) i o m i
await = PlanT (\kp _ kr kf -> kr kp kf)

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
awaits :: Functor (k i) => Handle k i j -> PlanT k i o m j
awaits f = PlanT $ \kp _ kr kf -> kr (fmap kp (f id)) kf

-- | @'stop' = 'empty'@
stop :: PlanT k i o m a
stop = empty
