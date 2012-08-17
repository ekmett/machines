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
  ) where

import Control.Applicative
import Control.Comonad
import Data.Machine.Plan
import Data.Machine.Type
import Data.Machine.Process

-- | 'Moore' machines
data Moore a b = Moore b (a -> Moore a b)

instance Automaton Moore where
  auto = construct . go where
    go (Moore b f) = do
      yield b
      await >>= go . f

instance Functor (Moore a) where
  fmap f (Moore b g) = Moore (f b) (fmap f . g)

instance Applicative (Moore a) where
  pure a = r where r = Moore a (const r)
  Moore f ff <*> Moore a fa  = Moore (f a) (\i -> ff i <*> fa i)
  m <* _ = m
  _ *> n = n

instance Comonad (Moore a) where
  extract (Moore b _) = b
  extend f w@(Moore _ g) = Moore (f w) (extend f . g)

instance ComonadApply (Moore a) where
  Moore f ff <@> Moore a fa = Moore (f a) (\i -> ff i <*> fa i)
  m <@ _ = m
  _ @> n = n
