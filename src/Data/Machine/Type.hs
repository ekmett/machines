{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x,y,z) 0
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Type
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  rank-N, GADTs
--
----------------------------------------------------------------------------
module Data.Machine.Type
  (
  -- * Machines
    Machine(..)
  , run_
  , run
  -- ** Building machines from plans
  , repeatedly
  -- * Reshaping machines
  , fit
  , pass
  ) where

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Data.Foldable
import Data.Functor.Plus
import Data.Functor.Bind
import Data.Machine.Await
import Data.Pointed
import Data.Semigroup
import Prelude hiding ((.),id)

-------------------------------------------------------------------------------
-- Machines
-------------------------------------------------------------------------------

newtype Machine m a = Machine
  { runMachine :: forall r.
      (a -> r -> r) ->                          -- Yield a (Machine m a)
      r ->                                      -- Halt
      (forall z. m z -> r -> (z -> r) -> r) ->  -- forall z. Await (m z) (Machine m a) (z -> Machine m a)
      r
  }

-- | A @'Machine' o m a@ is a specification for a pure 'Machine', that can perform actions in @m@, which
-- writes values of type @o@, and has intermediate results of type @a@.
--
-- It is perhaps easier to think of 'Machine' in its un-cps'ed form, which would
-- look like:

instance Functor (Machine m) where
  fmap f (Machine m) = Machine $ \k -> m (k . f)
  {-# INLINE fmap #-}

instance Apply (Machine m) where
  (<.>) = ap
  {-# INLINE (<.>) #-}

instance Applicative (Machine m) where
  pure a = Machine $ \ky kh _ -> ky a kh
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Alt (Machine m) where
  (<!>) = (<|>)
  {-# INLINE (<!>) #-}

instance Plus (Machine m) where
  zero = empty
  {-# INLINE zero #-}

instance Alternative (Machine m) where
  empty = Machine $ \_ kh _ -> kh
  {-# INLINE empty #-}
  Machine m <|> Machine n = Machine $ \ky kh ka -> m ky (n ky kh ka) ka
  {-# INLINE (<|>) #-}

instance Bind (Machine m) where
  (>>-) = (>>=)
  {-# INLINE (>>-) #-}

instance Monad (Machine m) where
  return a = Machine $ \ky kh _ -> ky a kh
  {-# INLINE return #-}
  Machine m >>= f = Machine $ \ky kh ka -> m (\a kn -> runMachine (f a) ky kn ka) kh ka
  fail _ = Machine $ \_ kh _ -> kh
  {-# INLINE (>>=) #-}

instance MonadPlus (Machine m) where
  mzero = empty
  {-# INLINE mzero #-}
  mplus = (<|>)
  {-# INLINE mplus #-}

instance MonadTrans Machine where
  lift m = Machine $ \ky kh ka -> ka m kh $ \a -> ky a kh
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (Machine m) where
  liftIO m = Machine $ \ky kh ka -> ka (liftIO m) kh $ \a -> ky a kh
  {-# INLINE liftIO #-}

instance MonadState s m => MonadState s (Machine m) where
  get = lift get
  {-# INLINE get #-}
  put = lift . put
  {-# INLINE put #-}
#if MIN_VERSION_mtl(2,1,0)
  state = lift . state
  {-# INLINE state #-}
#endif

instance MonadReader e m => MonadReader e (Machine m) where
  ask = lift ask
  {-# INLINE ask #-}
#if MIN_VERSION_mtl(2,1,0)
  reader = lift . reader
  {-# INLINE reader #-}
#endif
  local f (Machine m) = Machine $ \ky kh ka -> m ky kh (ka . local f)
  {-# INLINE local #-}

instance Await a m => Await a (Machine m) where
  await = awaits await
  {-# INLINE await #-}

--- | Wait for a particular input.
---
--- @
--- 'awaits' 'L' :: 'Await' i f => 'Machine' o (f :+: g) i
--- 'awaits' 'R' :: 'Await' j g => 'Machine' o (f :+: g) j
--- 'awaits' 'This' :: 'Await' i f => 'Machine' o (Y f g) i
--- 'awaits' 'That' :: 'Await' j g => 'Machine' o (Y f g) j
--- @
awaits :: Await a m => (m a -> n b) -> Machine n b
awaits f = request (f await)
{-# INLINE awaits #-}

request :: m a -> Machine m a
request m = Machine $ \ky kh ka -> ka m kh $ \a -> ky a kh
{-# INLINE request #-}

instance Pointed (Machine m) where
  point a = Machine $ \ky kh _ -> ky a kh
  {-# INLINE point #-}

instance Semigroup (Machine m a) where
  (<>) = (<|>)
  {-# INLINE (<>) #-}

instance Monoid (Machine m a) where
  mempty = Machine $ \_ kh _ -> kh
  {-# INLINE mempty #-}
  mappend = (<|>)
  {-# INLINE mappend #-}

instance Foldable (Machine m) where
  foldMap f m = runMachine m (\a r -> f a `mappend` r) mempty (\_ e _ -> e)
  {-# INLINE foldMap #-}

-------------------------------------------------------------------------------
-- Transduction Machines
-------------------------------------------------------------------------------

run_ :: MonadPlus m => Machine m b -> m ()
run_ m = runMachine m (\a r -> r) (return ()) $ \m ke ks -> mplus (m >>= ks) ke
{-# INLINE run_ #-}

run :: MonadPlus m => Machine m b -> m [b]
run m = runMachine m (\a r -> (a:) `liftM` r) (return []) $ \m ke ks -> mplus (m >>= ks) ke
{-# INLINE run #-}

-- |
-- Connect different kinds of machines.
--
-- @
-- 'fit' 'id' = 'id'
-- 'fit' 'Data.Machine.Tee.L' :: 'Machine' (f 'Data.Machine.Tee.:+:' g) a -> 'Machine' f a
-- 'fit' 'Data.Machine.Tee.R' :: 'Machine' (f 'Data.Machine.Tee.:+:' g) a -> 'Machine' g a
-- 'fit' 'Data.Machine.Wye.This' :: 'Machine' ('Data.Machine.Wye.Y' f g) a -> 'Machine' f a
-- 'fit' 'Data.Machine.Wye.That' :: 'Machine' ('Data.Machine.Wye.Y' f g) a -> 'Machine' g a
-- @
fit :: (forall a. m a -> n a) -> Machine m o -> Machine n o
fit f (Machine m) = Machine $ \ky kh ka -> m ky kh (ka . f)
{-# INLINE fit #-}

-- | Generates a model that runs a 'Machine' until it stops, then start it up again.
--
-- @'repeatedly' m = 'Control.Monad.forever' m@
repeatedly :: Machine m a -> Machine m a
repeatedly m = fix (m <>)
{-# INLINE repeatedly #-}

-- | Repeatedly 'request' the same thing and 'return' it.
--
-- @
-- 'pass' 'id' :: 'Data.Machine.Process.Process' a a
-- @
pass :: m o -> Machine m o
pass = repeatedly . request
{-# INLINE pass #-}
