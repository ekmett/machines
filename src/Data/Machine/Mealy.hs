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

import Control.Arrow
import Control.Category
import Data.Machine.Plan
import Data.Machine.Type
import Data.Machine.Process
import Prelude hiding ((.),id)

-- | 'Mealy' machines
newtype Mealy a b = Mealy { runMealy :: a -> (b, Mealy a b) }

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
