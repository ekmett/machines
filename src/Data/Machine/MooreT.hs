{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.MooreT
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- <http://en.wikipedia.org/wiki/Moore_machine>
----------------------------------------------------------------------------
module Data.Machine.MooreT
  ( MooreT(..)
  , unfoldMooreT
  , upgrade
  , hoist
  , couple
  , firstM
  , secondM
  ) where

import Control.Monad.Trans (lift)
import Data.Distributive   (Distributive(..), cotraverse)
import Data.Machine
import Data.Machine.MealyT (MealyT(runMealyT))
import Data.Pointed        (Pointed(..))
import Data.Profunctor     (Costrong(..), Profunctor(..))

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup      (Semigroup(..))
#endif

-- | 'Moore' machine, with applicative effects
newtype MooreT m a b = MooreT { runMooreT :: m (b, a -> MooreT m a b) }

-- | Construct a MooreT machine from a state valuation and transition action
unfoldMooreT :: Functor m => (s -> m (b, a -> s)) -> s -> MooreT m a b
unfoldMooreT f = go where
  go s = MooreT $ (\(b, k) -> (b, go . k)) <$> f s
{-# INLINE unfoldMooreT #-}

upgrade :: Applicative m => Moore a b -> MooreT m a b
upgrade (Moore b f) = MooreT $ pure (b, upgrade . f)
{-# INLINE upgrade #-}

firstM :: (Functor m, Monad m) => (a' -> m a) -> MooreT m a b -> MooreT m a' b
firstM f = MooreT .  fmap (fmap go) . runMooreT
  where
    go m x = MooreT $ f x >>= fmap (fmap go) . runMooreT . m
{-# INLINE firstM #-}

secondM :: Monad m => (b -> m b') -> MooreT m a b -> MooreT m a b'
secondM f m = MooreT $ do
  (b, m') <- runMooreT m
  b' <- f b
  return (b', secondM f . m')
{-# INLINE secondM #-}

hoist :: Functor n => (forall x. m x -> n x) -> MooreT m a b -> MooreT n a b
hoist f = let go = MooreT . fmap (\(b, m') -> (b, go . m')) . f . runMooreT in go
{-# INLINE hoist #-}

couple :: Monad m => MooreT m a b -> MealyT m b a -> m c
couple x y = do
  (b, x') <- runMooreT x
  (a, y') <- runMealyT y b
  couple (x' a) y'
{-# INLINE couple #-}

instance AutomatonM MooreT where
  autoT = construct . go where
    go m = do
      (b, m') <- lift (runMooreT m)
      yield b
      await >>= go . m'
  {-# INLINE autoT #-}

instance Functor m => Functor (MooreT m a) where
  fmap f = let go = MooreT . fmap (\(b, m') -> (f b, go . m')) . runMooreT in go
  {-# INLINE fmap #-}

instance Functor m => Profunctor (MooreT m) where
  rmap = fmap
  {-# INLINE rmap #-}
  lmap f = let go = MooreT . fmap (\(b, m') -> (b, go . m' . f)) . runMooreT in go
  {-# INLINE lmap #-}
  dimap f g = let go = MooreT . fmap (\(b, m') -> (g b, go . m' . f)) . runMooreT in go
  {-# INLINE dimap #-}

instance Applicative m => Applicative (MooreT m a) where
  pure x = let r = MooreT $ pure (x, const r) in r
  {-# INLINE pure #-}
  fm <*> xm = MooreT $
    (\(f, fm') (x, xm') -> (f x, \a -> fm' a <*> xm' a)) <$> runMooreT fm <*> runMooreT xm
  {-# INLINE (<*>) #-}

instance Applicative m => Pointed (MooreT m a) where
  point = pure
  {-# INLINE point #-}

instance (Functor m, Monad m) => Costrong (MooreT m) where
  unfirst m = MooreT $ do
    ((b, d), m') <- runMooreT m
    return (b, \a -> unfirst $ m' (a, d))
  {-# INLINE unfirst #-}
  unsecond m = MooreT $ do
    ((d, b), m') <- runMooreT m
    return (b, \a -> unsecond $ m' (d, a))
  {-# INLINE unsecond #-}

instance (Distributive m, Applicative m) => Distributive (MooreT m a) where
  distribute m = MooreT $
    cotraverse (\x -> (fmap fst x, fmap distribute $ distribute $ fmap snd x))
    $ fmap runMooreT m
  {-# INLINE distribute #-}

instance (Applicative m, Semigroup b) => Semigroup (MooreT m a b) where
  a <> b = (<>) <$> a <*> b
  {-# INLINE (<>) #-}

instance (Applicative m, Monoid b) => Monoid (MooreT m a b) where
  mempty = pure mempty
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  mappend a b = mappend <$> a <*> b
  {-# INLINE mappend #-}
#endif
