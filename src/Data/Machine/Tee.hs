{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Tee
-- Copyright   :  (C) 2012 Edward Kmett, Rúnar Bjarnason, Paul Chiusano
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank-2 Types, GADTs
--
----------------------------------------------------------------------------
module Data.Machine.Tee
  ( -- * Tees
    Tee
  , (:+:)(..)
  , tee
  , addL, addR
  , capL, capR
  ) where

import Control.Applicative
import Control.Comonad
import Data.Foldable
import Data.Functor.Extend
import Data.Machine.Type
import Data.Machine.Source
import Data.Copointed
import Data.Semigroup.Traversable
import Data.Semigroup.Foldable
import Data.Traversable

-------------------------------------------------------------------------------
-- Tees
-------------------------------------------------------------------------------

-- | The input descriptor for a 'Tee' or 'TeeT'.
data (m :+: n) a = L (m a) | R (n a)
  deriving (Functor, Foldable, Traversable)

instance (Foldable1 f, Foldable1 g) => Foldable1 (f :+: g) where
  foldMap1 f (L a) = foldMap1 f a
  foldMap1 f (R b) = foldMap1 f b

instance (Traversable1 f, Traversable1 g) => Traversable1 (f :+: g) where
  traverse1 f (L a) = L <$> traverse1 f a
  traverse1 f (R b) = R <$> traverse1 f b

instance (Copointed m, Copointed n) => Copointed (m :+: n) where
  copoint (L m) = copoint m
  copoint (R m) = copoint m

instance (Extend m, Extend n) => Extend (m :+: n) where
  duplicated (L m) = L (extended L m)
  duplicated (R m) = R (extended R m)
  extended f (L m) = L (extended (f . L) m)
  extended f (R m) = R (extended (f . R) m)

instance (Comonad m, Comonad n) => Comonad (m :+: n) where
  extract (L m)   = extract m
  extract (R m)   = extract m
  duplicate (L m) = L (extend L m)
  duplicate (R m) = R (extend R m)
  extend f (L m)  = L (extend (f . L) m)
  extend f (R m)  = R (extend (f . R) m)

-- | A 'Machine' that can read from two input stream in a deterministic manner.
type Tee a b = Machine ((->) a :+: (->) b)

-- | Compose a pair of pipes onto the front of a Tee.
tee :: Machine m a -> Machine n b -> Tee a b c -> Machine (m :+: n) c
tee ma mb m = case m of
  Stop         -> Stop
  Yield o k    -> Yield o $ tee ma mb k
  Await f (L fh) ff -> case ma of
    Stop            -> tee empty mb ff
    Yield a k       -> tee k mb (f (fh a))
    Await g h fg -> Await (\a -> tee (g a) mb m) (L h) (tee fg mb m)
  Await f (R fh) ff -> case mb of
    Stop            -> tee ma empty ff
    Yield b k       -> tee ma k (f (fh b))
    Await g h fg -> Await (\b -> tee ma (g b) m) (R h) (tee ma fg m)

-- | Precompose a machine onto the left input of a tee-like.
addL :: Machine m a -> Machine ((->) a :+: n) b -> Machine (m :+: n) b
addL ma m = case m of
  Stop             -> Stop
  Yield o k        -> Yield o $ addL ma k
  Await f (L fh) ff -> case ma of
    Stop         -> addL empty ff
    Yield a k    -> addL k (f (fh a))
    Await g h fg -> Await (\a -> addL (g a) m) (L h) (addL fg m)
  Await f (R fh) ff -> Await (addL ma . f) (R fh) (addL ma ff)

-- | Precompose a machine onto the right input of a tee-like.

-- Alternatively: addR ma = fit flipT . addL ma . fit flipT
addR :: Machine m a -> Machine (n :+: (->) a) b -> Machine (n :+: m) b
addR ma m = case m of
  Stop             -> Stop
  Yield o k        -> Yield o $ addR ma k
  Await f (L fh) ff -> Await (addR ma . f) (L fh) (addR ma ff)
  Await f (R fh) ff -> case ma of
    Stop         -> addR empty ff
    Yield a k    -> addR k (f (fh a))
    Await g h fg -> Await (\a -> addR (g a) m) (R h) (addR fg m)

-- | Tie off one input of a tee-like by connecting it to a known source.
capL :: Source a -> Machine ((->) a :+: m) b -> Machine m b
capL s t = fit cappedT (addL s t)
{-# INLINE capL #-}

-- | Tie off one input of a tee-like by connecting it to a known source.
capR :: Source a -> Machine (m :+: (->) a) b -> Machine m b
capR s t = fit cappedT (addR s t)
{-# INLINE capR #-}

-- | Natural transformation used by 'capL' and 'capR'.
cappedT :: (f :+: f) a -> f a
cappedT (R f) = f
cappedT (L f) = f
{-# INLINE cappedT #-}
