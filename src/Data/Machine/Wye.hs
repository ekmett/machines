{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Wye
-- Copyright   :  (C) 2012 Edward Kmett, Rúnar Bjarnason, Paul Chiusano
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  rank-2
--
----------------------------------------------------------------------------
module Data.Machine.Wye
  (
  -- * Wyes
    Wye, WyeT
  , Y(..)
  , wye
  , addX, addY, addZ
  -- , capX, capY, capZ
  ) where

import Control.Category
import Data.Machine.Process
import Data.Machine.Type
import Data.Machine.Source
import Prelude hiding ((.),id)

-------------------------------------------------------------------------------
-- Wyes
-------------------------------------------------------------------------------

-- | The input descriptor for a 'Wye' or 'WyeT'
data Y i c where
  X :: (a -> c) -> Y (Either a b) c          -- block waiting on the left input
  Y :: (b -> c) -> Y (Either a b) c          -- block waiting on the right input
  Z :: (Either a b -> c) -> Y (Either a b) c -- block waiting on either input

instance Functor (Y i) where
  fmap f (X k) = X (f . k)
  fmap f (Y k) = Y (f . k)
  fmap f (Z k) = Z (f . k)

-- | A 'Machine' that can read from two input stream in a deterministic manner.
type Wye a b c = Machine Y (Either a b) c

-- | A 'Machine' that can read from two input stream in a deterministic manner with monadic side-effects.
type WyeT m a b c = MachineT m Y (Either a b) c

-- | Compose a pair of pipes onto the front of a Wye.
wye :: Monad m => ProcessT m a a' -> ProcessT m b b' -> WyeT m a' b' c -> WyeT m a b c
wye ma mb m = MachineT $ runMachineT m >>= \v -> case v of
  Stop                -> return Stop
  Yield o k           -> return $ Yield o (wye ma mb k)
  Await f (X kf) ff   -> runMachineT ma >>= \u -> case u of
    Stop                -> runMachineT $ wye stopped mb ff
    Yield a k           -> runMachineT $ wye k mb (fmap f kf a)
    Await g kg fg
      | mv <- encased v ->
      return $ Await id (X (\a -> wye (fmap g kg a) mb mv)) (wye fg mb mv)
  Await f (Y kf) ff   -> runMachineT mb >>= \u -> case u of
    Stop                -> runMachineT $ wye ma stopped ff
    Yield b k           -> runMachineT $ wye ma k (fmap f kf b)
    Await g kg fg
      | mv <- encased v ->
      return $ Await id (Y (\b -> wye ma (fmap g kg b) mv)) (wye ma fg mv)
  Await f (Z kf) ff   -> runMachineT ma >>= \u -> case u of
    Stop                -> runMachineT mb >>= \w -> case w of
      Stop                -> runMachineT $ wye stopped stopped ff
      Yield b k           -> runMachineT $ wye stopped k (fmap (f . Right) kf b)
      Await g kg fg
        | mv <- encased v -> return $ Await id (Y (\b -> wye stopped (fmap g kg b) mv)) (wye stopped fg mv)
    Yield a k           -> runMachineT $ wye k mb (fmap f kf a)
    Await g kg fg       -> runMachineT mb >>= \w -> case w of
      Stop
        | mv <- encased v -> return $ Await id (X (\a -> wye (fmap g kg a) stopped mv)) (wye fg stopped mv)
      Yield b k           -> runMachineT $ wye (encased u) k (fmap (f . Right) kf b)
      Await h kh fh
        | mv <- encased v ->
        return $ Await
          id
          (Z (\c -> case c of
            Left a  -> wye (fmap g kg a) (encased w) mv
            Right b -> wye (encased u) (fmap h kh b) mv))
          (wye fg fh mv)

-- | Precompose a pipe onto the left input of a wye.
addX :: Monad m => ProcessT m a b -> WyeT m b c d -> WyeT m a c d
addX p = wye p id

-- | Precompose a pipe onto the right input of a tee.
addY :: Monad m => ProcessT m b c -> WyeT m a c d -> WyeT m a b d
addY = wye id

addZ :: Monad m => ProcessT m a b -> WyeT m b b c -> WyeT m a a c
addZ p = wye p p

{-
-- | Tie off one input of a tee by connecting it to a known source.
capL :: Monad m => SourceT m a -> WyeT m a b c -> ProcessT m b c
capL s t = fitting capped (addL s t)

-- | Tie off one input of a tee by connecting it to a known source.
capR :: Monad m => SourceT m b -> WyeT m a b c -> ProcessT m a c
capR s t = fitting capped (addR s t)

-- | Natural transformation used by 'capL' and 'capR'.
capped :: Merge (Either a a) b -> a -> b
capped (X r) = r
capped (Y r) = r
capped (Z r) = \a -> case a of
  Left i ->

-}
