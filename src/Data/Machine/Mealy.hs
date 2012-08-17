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
----------------------------------------------------------------------------
module Data.Machine.Mealy
  ( Mealy(..)
  ) where

-- import Control.Category
import Data.Machine.Plan
import Data.Machine.Type
import Data.Machine.Process

-- | 'Mealy' machines
newtype Mealy a b = Mealy { runMealy :: a -> (b, Mealy a b) }

instance Automaton Mealy where
  auto = construct . loop where
    loop (Mealy f) = await >>= \a -> case f a of
      (b, m) -> do
         yield b
         loop m
