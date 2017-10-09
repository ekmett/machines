{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Stack
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  GADTs
--
----------------------------------------------------------------------------
module Data.Machine.Stack
  ( Stack(..)
  , stack
  , peek
  , pop
  , push
  ) where

import Data.Machine.Is
import Data.Machine.Plan
import Data.Machine.Type

-- | This is a simple process type that knows how to push back input.
data Stack a r where
  Push :: a -> Stack a ()
  Pop  ::      Stack a a

-- | Peek at the next value in the input stream without consuming it
peek :: Plan (Stack a) b a
peek = do
  a <- pop
  push a
  return a
{-# INLINABLE peek #-}

-- | Push back into the input stream
push :: a -> Plan (Stack a) b ()
push a = awaits (Push a)
{-# INLINABLE push #-}

-- | Pop the next value in the input stream
pop :: Plan (Stack a) b a
pop = awaits Pop
{-# INLINABLE pop #-}

-- | Stream outputs from one 'Machine' into another with the possibility
-- of pushing inputs back.
stack :: forall m a. (Monad m) => TranslateT m (Stack a) (Is a)
stack = TranslateT $ \req -> case req of
  Pop -> return $ Await (\i -> return $ Yield i stack) Refl (return Stop)
  Push a -> go a
  where
    go :: a -> MStep m (Stack a) (Is a) ()
    go a = return $ Yield () $ TranslateT $ \req -> case req of
      Pop -> return $ Yield a stack
      Push b -> go b
{-# INLINABLE stack #-}
