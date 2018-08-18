{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Mealy
-- License     :  BSD-style (see the file LICENSE)
--
-- <http://en.wikipedia.org/wiki/Mealy_machine>
-- <https://github.com/ivanperez-keera/dunai/blob/develop/src/Data/MonadicStreamFunction/Core.hs#L35>
-- <https://hackage.haskell.org/package/auto-0.4.3.0/docs/Control-Auto.html>
-- <https://hackage.haskell.org/package/varying-0.6.0.0/docs/Control-Varying-Core.html>
----------------------------------------------------------------------------
module Data.Machine.MealyT
  ( MealyT(..)
  , arrPure
  , arrM
  , upgrade
  , scanMealyT
  , scanMealyTM
  ) where

import Data.Machine
import Control.Arrow
import Control.Applicative
import Control.Monad.Trans
import Data.Pointed
import Control.Monad.Identity
import Data.Profunctor
import Data.Semigroup
import qualified Control.Category as C
import Prelude

-- | 'Mealy' machine, with applicative effects
newtype MealyT m a b = MealyT { runMealyT :: a -> m (b, MealyT m a b) }

instance Functor m => Functor (MealyT m a) where
  {-# INLINE fmap #-}
  fmap f (MealyT m) = MealyT $ \a ->
    fmap (\(x,y) -> (f x, fmap f y)) (m a)

instance Pointed m => Pointed (MealyT m a) where
  {-# INLINE point #-}
  point b = r where r = MealyT (const (point (b, r)))

instance Applicative m => Applicative (MealyT m a) where
  {-# INLINE pure #-}
  pure b = r where r = MealyT (const (pure (b, r))) -- Stolen from Pointed
  MealyT m <*> MealyT n = MealyT $ \a -> (\(mb, mm) (nb, nm) -> (mb nb, mm <*> nm)) <$> m a <*> n a

instance Functor m => Profunctor (MealyT m) where
  rmap = fmap
  {-# INLINE rmap #-}
  lmap f = go where
    go (MealyT m) = MealyT $ \a -> fmap (\(b,n) -> (b, go n)) (m (f a))
  {-# INLINE lmap #-}
#if MIN_VERSION_profunctors(3,1,1)
  dimap f g = go where
    go (MealyT m) = MealyT $ \a -> fmap (\(b,n) -> (g b, go n)) (m (f a))
  {-# INLINE dimap #-}
#endif

instance Monad m => C.Category (MealyT m) where
  {-# INLINE id #-}
  id = MealyT $ \a -> return (a, C.id)
  MealyT bc . MealyT ab = MealyT $ \a ->
    do (b, nab) <- ab a
       (c, nbc) <- bc b
       return (c, nbc C.. nab)

instance Monad m => Arrow (MealyT m) where
  {-# INLINE arr #-}
  arr f = r where r = MealyT (\a -> return (f a, r))
  first (MealyT m) = MealyT $ \(a,c) ->
    do (b, n) <- m a
       return ((b, c), first n)

arrPure :: (a -> b) -> MealyT Identity a b
arrPure = arr

arrM :: Functor m => (a -> m b) -> MealyT m a b
arrM f = r where r = MealyT $ \a -> fmap (,r) (f a)

upgrade :: Applicative m => Mealy a b -> MealyT m a b
upgrade (Mealy f) = MealyT $ \a -> let (r, g) = f a in pure (r, upgrade g)

scanMealyT :: Applicative m => (a -> b -> a) -> a -> MealyT m b a
scanMealyT f a = MealyT (\b -> pure (a, scanMealyT f (f a b)))

scanMealyTM :: Functor m => (a -> b -> m a) -> a -> MealyT m b a
scanMealyTM f a = MealyT $ \b -> (\x -> (a, scanMealyTM f x)) <$> f a b

autoMealyTImpl :: Monad m => MealyT m a b -> ProcessT m a b
autoMealyTImpl = construct . go
  where
  go (MealyT f) = do
    a      <- await
    (b, m) <- lift $ f a
    yield b
    go m

instance AutomatonM MealyT where
  autoT = autoMealyTImpl

instance (Semigroup b, Applicative m) => Semigroup (MealyT m a b) where
  f <> g = MealyT $ \x ->
    (\(fx, f') (gx, g') -> (fx <> gx, f' <> g')) <$> runMealyT f x <*> runMealyT g x

instance (Semigroup b, Monoid b, Applicative m) => Monoid (MealyT m a b) where
  mempty = MealyT $ \_ -> pure mempty
  mappend = (<>)
