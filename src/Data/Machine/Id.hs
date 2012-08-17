-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Id
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Machine.Id
  ( Id(..)
  ) where

import Control.Applicative
import Control.Comonad
import Data.Foldable
import Data.Traversable

newtype Id a = Id { runId :: a }

instance Functor Id where
  fmap f (Id a) = Id (f a)

instance Comonad Id where
  extract (Id a) = a
  extend f w = Id (f w)

instance ComonadApply Id where
  Id f <@> Id a = Id (f a)
  m <@ _ = m
  _ @> m = m

instance Foldable Id where
  foldMap f (Id a) = f a

instance Traversable Id where
  traverse f (Id a) = Id <$> f a

instance Applicative Id where
  pure = Id
  Id f <*> Id a = Id (f a)
  m <* _ = m
  _ *> m = m

instance Monad Id where
  return = Id
  Id a >>= f = f a
