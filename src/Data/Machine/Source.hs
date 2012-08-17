{-# LANGUAGE Rank2Types, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Source
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  rank-2
--
----------------------------------------------------------------------------
module Data.Machine.Source
  (
  -- * Sources
    Source, SourceT
  , source
  , repeated
  , cycled
  , cap
  ) where

import Control.Category
import Data.Foldable
import Data.Machine.Plan
import Data.Machine.Type
import Data.Machine.Process
import Prelude hiding ((.),id)

-------------------------------------------------------------------------------
-- Source
-------------------------------------------------------------------------------

-- | A 'Source' never reads from its inputs.
type Source b = forall k a. Machine k a b

-- | A 'SourceT' never reads from its inputs, but may have monadic side-effects.
type SourceT m b = forall k a. MachineT m k a b

-- | Repeat the same value, over and over.
repeated :: o -> Source o
repeated = repeatedly . yield

-- | Loop through a 'Foldable' container over and over.
cycled :: Foldable f => f b -> Source b
cycled xs = repeatedly (traverse_ yield xs)

-- | Generate a 'Source' from any 'Foldable' container.
source :: Foldable f => f b -> Source b
source xs = construct (traverse_ yield xs)

-- |
-- You can transform a 'Source' with a 'Process'.
--
-- Alternately you can view this as capping the 'Source' end of a 'Process',
-- yielding a new 'Source'.
--
-- @'cap' = 'pipe'@
--
cap :: Process a b -> Source a -> Source b
cap l r = after r l
