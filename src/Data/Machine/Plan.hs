{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x,y,z) 0
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Plan
-- Copyright   :  (C) 2012-2013 Edward Kmett, Rúnar Bjarnason
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  MPTCs
--
----------------------------------------------------------------------------
module Data.Machine.Plan
  (
  -- * Plans
    Plan(..)
  , await
  , awaits
  , request
  ) where

import Control.Applicative
import Control.Category
import Control.Monad (ap, MonadPlus(..))
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Data.Functor.Bind
import Data.Functor.Plus
import Data.Machine.Await
import Prelude hiding ((.),id)

-------------------------------------------------------------------------------
-- Plans
-------------------------------------------------------------------------------

-- | You can 'construct' a 'Plan', turning it into a
-- 'Data.Machine.Type.Machine'.
--
newtype Plan m a = Plan
  { runPlan :: forall r.
      (a -> r -> r) ->                          -- Yield a (Plan m a)
      r ->                                      -- Halt
      (forall z. m z -> r -> (z -> r) -> r) ->  -- forall z. Await (m z) (Plan m a) (z -> Plan m a)
      r
  }

-- | A @'Plan' o m a@ is a specification for a pure 'Machine', that can perform actions in @m@, which
-- writes values of type @o@, and has intermediate results of type @a@.
--
-- It is perhaps easier to think of 'Plan' in its un-cps'ed form, which would
-- look like:

instance Functor (Plan m) where
  fmap f (Plan m) = Plan $ \k -> m (k . f)
  {-# INLINE fmap #-}

instance Apply (Plan m) where
  (<.>) = ap
  {-# INLINE (<.>) #-}

instance Applicative (Plan m) where
  pure a = Plan $ \ky kh _ -> ky a kh
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Alt (Plan m) where
  (<!>) = (<|>)
  {-# INLINE (<!>) #-}

instance Plus (Plan m) where
  zero = empty
  {-# INLINE zero #-}

instance Alternative (Plan m) where
  empty = Plan $ \_ kh _ -> kh
  {-# INLINE empty #-}
  Plan m <|> Plan n = Plan $ \ky kh ka -> m ky (n ky kh ka) ka
  {-# INLINE (<|>) #-}

instance Bind (Plan m) where
  (>>-) = (>>=)
  {-# INLINE (>>-) #-}

instance Monad (Plan m) where
  return a = Plan $ \ky kh _ -> ky a kh
  {-# INLINE return #-}
  Plan m >>= f = Plan $ \ky kh ka -> m (\a kn -> runPlan (f a) ky kn ka) kh ka
  fail _ = Plan $ \_ kh _ -> kh
  {-# INLINE (>>=) #-}

instance MonadPlus (Plan m) where
  mzero = empty
  {-# INLINE mzero #-}
  mplus = (<|>)
  {-# INLINE mplus #-}

instance MonadTrans Plan where
  lift m = Plan $ \ky kh ka -> ka m kh $ \a -> ky a kh
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (Plan m) where
  liftIO m = Plan $ \ky kh ka -> ka (liftIO m) kh $ \a -> ky a kh
  {-# INLINE liftIO #-}

instance MonadState s m => MonadState s (Plan m) where
  get = lift get
  {-# INLINE get #-}
  put = lift . put
  {-# INLINE put #-}
#if MIN_VERSION_mtl(2,1,0)
  state = lift . state
  {-# INLINE state #-}
#endif

instance MonadReader e m => MonadReader e (Plan m) where
  ask = lift ask
  {-# INLINE ask #-}
#if MIN_VERSION_mtl(2,1,0)
  reader = lift . reader
  {-# INLINE reader #-}
#endif
  local f (Plan m) = Plan $ \ky kh ka -> m ky kh (ka . local f)
  {-# INLINE local #-}

instance Await i m => Await i (Plan m) where
  await = awaits await
  {-# INLINE await #-}

--- | Wait for a particular input.
---
--- @
--- 'awaits' 'L' :: 'Await' i f => 'Plan' o (f :+: g) i
--- 'awaits' 'R' :: 'Await' j g => 'Plan' o (f :+: g) j
--- 'awaits' 'This' :: 'Await' i f => 'Plan' o (Y f g) i
--- 'awaits' 'That' :: 'Await' j g => 'Plan' o (Y f g) j
--- @
awaits :: Await i f => (f i -> g j) -> Plan g j
awaits f = request (f await)
{-# INLINE awaits #-}

request :: m a -> Plan m a
request m = Plan $ \ky kh ka -> ka m kh $ \a -> ky a kh
{-# INLINE request #-}
