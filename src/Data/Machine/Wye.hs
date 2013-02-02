{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Wye
-- Copyright   :  (C) 2012-2013 Edward Kmett, Rúnar Bjarnason, Paul Chiusano
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank-2 Types, GADTs
--
----------------------------------------------------------------------------
module Data.Machine.Wye
  (
  -- * Wyes
    Wye
  , Y(..)
  , wye
  , addX, addY
  , capX, capY
  ) where

import Control.Applicative
import Control.Category
import Data.Foldable
import Data.Functor.Apply
import Data.Machine.Await
import Data.Machine.Process
import Data.Machine.Type
import Data.Machine.Source
import Data.Traversable
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Prelude hiding ((.),id)

-------------------------------------------------------------------------------
-- Wyes
-------------------------------------------------------------------------------

-- | The input descriptor for a 'Wye'.
data Y f g a = This (f a) | That (g a) | These (f a) (g a)
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

instance (Foldable1 f, Foldable1 g) => Foldable1 (Y f g) where
  foldMap1 f (This a) = foldMap1 f a
  foldMap1 f (That b) = foldMap1 f b
  foldMap1 f (These a b) = foldMap1 f a <> foldMap1 f b

instance (Traversable1 f, Traversable1 g) => Traversable1 (Y f g) where
  traverse1 f (This a)    = This <$> traverse1 f a
  traverse1 f (That b)    = That <$> traverse1 f b
  traverse1 f (These a b) = These <$> traverse1 f a <.> traverse1 f b

instance (Await i f, Await j g) => Await (Either i j) (Y f g) where
  await = These (Left <$> await) (Right <$> await)

-- | A 'Machine' that can read from two input stream in a non-deterministic manner.
type Wye a b = Machine ((->) a `Y` (->) b)

-- | Compose a pair of pipes onto the front of a 'Wye'.

-- | Precompose a 'Process' onto each input of a 'Wye'.
--
-- This is left biased in that it tries to draw values from 'This' whenever they are
-- available, and only draws from the 'That' input when 'This' would block.
wye :: Process a a' -> Process b b' -> Wye a' b' c -> Wye a b c
wye ma mb m = case m of
  Yield o k           -> Yield o (wye ma mb k)
  Stop                -> Stop
  Await f (This fh) ff  -> case ma of
    Yield a k           -> wye k mb (f $ fh a)
    Stop                -> wye Stop mb ff
    Await g h fg        -> Await (\a -> wye (g a) mb m) (This h) (wye fg mb m)
  Await f (That fh) ff  -> case mb of
    Yield b k           -> wye ma k (f $ fh b)
    Stop                -> wye ma Stop ff
    Await g h fg        -> Await (\b -> wye ma (g b) m) (That h) (wye ma fg m)
  Await f (These fx fy) ff -> case ma of
    Yield a k              -> wye k mb (f $ fx a)
    Stop                   -> case mb of
      Yield b k              -> wye Stop k (f $ fy b)
      Stop                   -> wye Stop Stop ff
      Await g h fg           -> Await (\b -> wye Stop (g b) m) (That h) (wye Stop fg m)
    Await g pg fg     -> case mb of
      Yield b k         -> wye ma k (f $ fy b)
      Stop              -> Await (\a -> wye (g a) Stop m) (This pg) (wye fg Stop m)
      Await h ph fh     -> Await (\c -> case c of
                                          Left a  -> wye (g a) mb m
                                          Right b -> wye ma (h b) m) (These (Left <$> pg) (Right <$> ph)) (wye fg fh m)

-- | Precompose a pipe onto the left input of a wye.
addX :: Process a b -> Wye b c d -> Wye a c d
addX p = wye p echo
{-# INLINE addX #-}

-- | Precompose a pipe onto the right input of a tee.
addY :: Process b c -> Wye a c d -> Wye a b d
addY = wye echo
{-# INLINE addY #-}

-- | Tie off one input of a tee by connecting it to a known source.
capX :: Source a -> Wye a b c -> Process b c
capX s t = fit (capped const) (addX s t)
{-# INLINE capX #-}

-- | Tie off one input of a tee by connecting it to a known source.
capY :: Source b -> Wye a b c -> Process a c
capY s t = fit (capped (const id)) (addY s t)
{-# INLINE capY #-}

-- | Natural transformation used by 'capX' and 'capY'
capped :: (f a -> f a -> f a) -> Y f f a -> f a
capped _ (This f)    = f
capped _ (That g)    = g
capped z (These f g) = z f g
{-# INLINE capped #-}
