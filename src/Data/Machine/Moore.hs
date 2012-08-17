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
----------------------------------------------------------------------------
module Data.Machine.Moore
  ( Moore(..)
  ) where

-- import Control.Category
import Data.Machine.Plan
import Data.Machine.Type
import Data.Machine.Process

-- | 'Moore' machines
data Moore a b = Moore b (a -> Moore a b)

instance Automaton Moore where
  auto = construct . loop where
    loop (Moore b f) = do
      yield b
      await >>= loop . f
