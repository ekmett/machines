{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_profunctors
#define MIN_VERSION_profunctors(x,y,z) 1
#endif
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
  , unfoldMealy
  , logMealy
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Machine.Plan
import Data.Machine.Type
import Data.Machine.Process
import Data.Profunctor
import Data.Pointed
import Data.Semigroup
import Data.Sequence as Seq
import Prelude hiding ((.),id)

-- | 'Mealy' machines
newtype Mealy a b = Mealy { runMealy :: a -> (b, Mealy a b) }

instance Functor (Mealy a) where
  fmap f (Mealy m) = Mealy $ \a -> case m a of
    (b, n) -> (f b, fmap f n)
  {-# INLINE fmap #-}
  b <$ _ = pure b
  {-# INLINE (<$) #-}

instance Applicative (Mealy a) where
  pure b = r where r = Mealy (const (b, r))
  {-# INLINE pure #-}
  Mealy m <*> Mealy n = Mealy $ \a -> case m a of
    (f, m') -> case n a of
       (b, n') -> (f b, m' <*> n')
  {-# INLINE (<*>) #-}
  m <* _ = m
  {-# INLINE (<*) #-}
  _ *> n = n
  {-# INLINE (*>) #-}

instance Pointed (Mealy a) where
  point b = r where r = Mealy (const (b, r))
  {-# INLINE point #-}

-- | A 'Mealy' machine modeled with explicit state.
unfoldMealy :: (s -> a -> (b, s)) -> s -> Mealy a b
unfoldMealy f = go where
  go s = Mealy $ \a -> case f s a of
    (b, t) -> (b, go t)
{-# INLINE unfoldMealy #-}

-- | slow diagonalization
instance Monad (Mealy a) where
  return b = r where r = Mealy (const (b, r))
  {-# INLINE return #-}
  m >>= f = Mealy $ \a -> case runMealy m a of
    (b, m') -> (fst (runMealy (f b) a), m' >>= f)
  {-# INLINE (>>=) #-}
  _ >> n = n
  {-# INLINE (>>) #-}

instance Profunctor Mealy where
  rmap = fmap
  {-# INLINE rmap #-}
  lmap f = go where
    go (Mealy m) = Mealy $ \a -> case m (f a) of
      (b, n) -> (b, go n)
  {-# INLINE lmap #-}
#if MIN_VERSION_profunctors(3,1,1)
  dimap f g = go where
    go (Mealy m) = Mealy $ \a -> case m (f a) of
      (b, n) -> (g b, go n)
  {-# INLINE dimap #-}
#endif

instance Automaton Mealy where
  auto = construct . go where
    go (Mealy f) = await >>= \a -> case f a of
      (b, m) -> do
         yield b
         go m
  {-# INLINE auto #-}

instance Category Mealy where
  id = Mealy (\a -> (a, id))
  Mealy bc . Mealy ab = Mealy $ \ a -> case ab a of
    (b, nab) -> case bc b of
      (c, nbc) -> (c, nbc . nab)

instance Arrow Mealy where
  arr f = r where r = Mealy (\a -> (f a, r))
  {-# INLINE arr #-}
  first (Mealy m) = Mealy $ \(a,c) -> case m a of
    (b, n) -> ((b, c), first n)

instance ArrowChoice Mealy where
  left m = Mealy $ \a -> case a of
    Left l  -> case runMealy m l of
      (b, m') -> (Left b, left m')
    Right r -> (Right r, left m)
  right m = Mealy $ \a -> case a of
    Left l -> (Left l, right m)
    Right r -> case runMealy m r of
      (b, m') -> (Right b, right m')
  m +++ n = Mealy $ \a -> case a of
    Left b -> case runMealy m b of
      (c, m') -> (Left c, m' +++ n)
    Right b -> case runMealy n b of
      (c, n') -> (Right c, m +++ n')
  m ||| n = Mealy $ \a -> case a of
    Left b -> case runMealy m b of
      (d, m') -> (d, m' ||| n)
    Right b -> case runMealy n b of
      (d, n') -> (d, m ||| n')

-- | Fast forward a mealy machine forward
driveMealy :: Mealy a b -> Seq a -> a -> (b, Mealy a b)
driveMealy m xs z = case viewl xs of
  y :< ys -> case runMealy m y of
    (_, n) -> driveMealy n ys z
  EmptyL  -> runMealy m z

-- | Accumulate history.
logMealy :: Semigroup a => Mealy a a
logMealy = Mealy $ \a -> (a, h a) where
  h a = Mealy $ \b -> let c = a <> b in (c, h c)
{-# INLINE logMealy #-}

instance ArrowApply Mealy where
  app = go Seq.empty where
    go xs = Mealy $ \(m,x) -> case driveMealy m xs x of
      (c, _) -> (c, go (xs |> x))
  {-# INLINE app #-}
