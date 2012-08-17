-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Mealy
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- <http://en.wikipedia.org/wiki/Mealy_machine>
----------------------------------------------------------------------------
module Data.Machine.Mealy
  ( Mealy(..)
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Machine.Plan
import Data.Machine.Type
import Data.Machine.Process
import Data.Profunctor
import Prelude hiding ((.),id)

-- | 'Mealy' machines
newtype Mealy a b = Mealy { runMealy :: a -> (b, Mealy a b) }

instance Functor (Mealy a) where
  fmap f (Mealy m) = Mealy $ \a -> case m a of
    (b, n) -> (f b, fmap f n)

instance Applicative (Mealy a) where
  pure b = r where r = Mealy (const (b, r))
  Mealy m <*> Mealy n = Mealy $ \a -> case m a of
    (f, m') -> case n a of
       (b, n') -> (f b, m' <*> n')
  m <* _ = m
  _ *> n = n

instance Profunctor Mealy where
  rmap = fmap
  lmap f (Mealy m) = Mealy $ \a -> case m (f a) of
    (b, n) -> (b, lmap f n)

instance Automaton Mealy where
  auto = construct . go where
    go (Mealy f) = await >>= \a -> case f a of
      (b, m) -> do
         yield b
         go m

instance Category Mealy where
  id = Mealy (\a -> (a, id))
  Mealy bc . Mealy ab = Mealy $ \ a -> case ab a of
    (b, nab) -> case bc b of
      (c, nbc) -> (c, nbc . nab)

instance Arrow Mealy where
  arr f = r where r = Mealy (\a -> (f a, r))
  first (Mealy m) = Mealy $ \(a,c) -> case m a of
    (b, n) -> ((b, c), first n)
