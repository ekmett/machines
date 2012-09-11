-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Moore
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- <http://en.wikipedia.org/wiki/Moore_machine>
----------------------------------------------------------------------------
module Data.Machine.Moore
  ( Moore(..)
  , logMoore
  , unfoldMoore
  ) where

import Control.Applicative
import Control.Comonad
import Control.Monad
import Data.Copointed
import Data.Machine.Plan
import Data.Machine.Type
import Data.Machine.Process
import Data.Monoid
import Data.Pointed
import Data.Profunctor

-- | 'Moore' machines
data Moore a b = Moore b (a -> Moore a b)

-- | Accumulate the input as a sequence.
logMoore :: Monoid m => Moore m m
logMoore = h mempty where
  h m = Moore m (\a -> h (m <> a))

-- | Construct a Moore machine from a state valuation and transition function
unfoldMoore :: (s -> (b, a -> s)) -> s -> Moore a b
unfoldMoore f = go where
  go s = case f s of
    (b, g) -> Moore b (go . g)

instance Automaton Moore where
  auto = construct . go where
    go (Moore b f) = do
      yield b
      await >>= go . f

instance Functor (Moore a) where
  fmap f (Moore b g) = Moore (f b) (fmap f . g)

instance Profunctor Moore where
  rmap = fmap
  lmap f (Moore b g) = Moore b (lmap f . g . f)

instance Applicative (Moore a) where
  pure a = r where r = Moore a (const r)
  Moore f ff <*> Moore a fa  = Moore (f a) (\i -> ff i <*> fa i)
  m <* _ = m
  _ *> n = n

instance Pointed (Moore a) where
  point a = r where r = Moore a (const r)

-- | slow diagonalization
instance Monad (Moore a) where
  return a = r where r = Moore a (const r)
  Moore a k >>= f = case f a of
    Moore b _ -> Moore b (k >=> f)
  _ >> m = m

instance Copointed (Moore a) where
  copoint (Moore b _) = b

instance Comonad (Moore a) where
  extract (Moore b _) = b
  extend f w@(Moore _ g) = Moore (f w) (extend f . g)

instance ComonadApply (Moore a) where
  Moore f ff <@> Moore a fa = Moore (f a) (\i -> ff i <*> fa i)
  m <@ _ = m
  _ @> n = n
