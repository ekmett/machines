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
  , addX, addY
  , capX, capY
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

-- | Compose a pair of pipes onto the front of a 'Wye'.

-- | Precompose a 'Process' onto each input of a 'Wye' (or 'WyeT').
--
-- This is left biased in that it tries to draw values from the 'X' input whenever they are
-- available, and only draws from the 'Y' input when 'X' would block.
wye :: Monad m => ProcessT m a a' -> ProcessT m b b' -> WyeT m a' b' c -> WyeT m a b c
wye ma mb m = MachineT $ runMachineT m >>= \v -> case v of
  Yield o k           -> return $ Yield o (wye ma mb k)
  Stop                -> return Stop
  Await f (X kf) ff   -> runMachineT ma >>= \u -> case u of
    Yield a k           -> runMachineT . wye k mb . f $ kf a
    Stop                -> runMachineT $ wye stopped mb ff
    Await g kg fg       -> return . Await id (X $ \a -> wye (g $ kg a) mb $ encased v)
                                  . wye fg mb $ encased v
  Await f (Y kf) ff   -> runMachineT mb >>= \u -> case u of
    Yield b k           -> runMachineT . wye ma k . f $ kf b
    Stop                -> runMachineT $ wye ma stopped ff
    Await g kg fg       -> return . Await id (Y $ \b -> wye ma (g $ kg b) $ encased v)
                                  . wye ma fg $ encased v
  Await f (Z kf) ff   -> runMachineT ma >>= \u -> case u of
    Yield a k           -> runMachineT . wye k mb . f . kf $ Left a
    Stop                -> runMachineT mb >>= \w -> case w of
      Yield b k           -> runMachineT . wye stopped k . f . kf $ Right b
      Stop                -> runMachineT $ wye stopped stopped ff
      Await g kg fg       -> return . Await id (Y $ \b -> wye stopped (g $ kg b) $ encased v)
                                    . wye stopped fg $ encased v
    Await g kg fg       -> runMachineT mb >>= \w -> case w of
      Yield b k           -> runMachineT . wye (encased u) k . f . kf $ Right b
      Stop                -> return . Await id (X $ \a -> wye (g $ kg a) stopped $ encased v)
                                    . wye fg stopped $ encased v
      Await h kh fh       -> return . Await id (Z $ \c -> case c of
                                                  Left a  -> wye (g $ kg a) (encased w) $ encased v
                                                  Right b -> wye (encased u) (h $ kh b) $ encased v)
                                    . wye fg fh $ encased v

-- | Precompose a pipe onto the left input of a wye.
addX :: Monad m => ProcessT m a b -> WyeT m b c d -> WyeT m a c d
addX p = wye p id

-- | Precompose a pipe onto the right input of a tee.
addY :: Monad m => ProcessT m b c -> WyeT m a c d -> WyeT m a b d
addY = wye id

-- | Tie off one input of a tee by connecting it to a known source.
capX :: Monad m => SourceT m a -> WyeT m a b c -> ProcessT m b c
capX s t = fitting cappedX (addX s t)

-- | Tie off one input of a tee by connecting it to a known source.
capY :: Monad m => SourceT m b -> WyeT m a b c -> ProcessT m a c
capY s t = fitting cappedY (addY s t)

-- | Natural transformation used by 'capY'
cappedX :: Y (Either a a) b -> a -> b
cappedX (X r) = r
cappedX (Y r) = r
cappedX (Z r) = r . Right

-- | Natural transformation used by 'capX'
cappedY :: Y (Either a a) b -> a -> b
cappedY (X r) = r
cappedY (Y r) = r
cappedY (Z r) = r . Left
