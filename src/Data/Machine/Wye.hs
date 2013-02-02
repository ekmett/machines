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

import Control.Category
import Data.Machine.Process
import Data.Machine.Type
import Data.Machine.Source
import Prelude hiding ((.),id)

-------------------------------------------------------------------------------
-- Wyes
-------------------------------------------------------------------------------

-- | The input descriptor for a 'Wye' or 'WyeT'
data Y a b c where
  X :: Y a b a            -- block waiting on the left input
  Y :: Y a b b            -- block waiting on the right input
  Z :: Y a b (Either a b) -- block waiting on either input

-- | A 'Machine' that can read from two input stream in a non-deterministic manner.
type Wye a b = Machine (Y a b)

-- | Compose a pair of pipes onto the front of a 'Wye'.

-- | Precompose a 'Process' onto each input of a 'Wye' (or 'WyeT').
--
-- This is left biased in that it tries to draw values from the 'X' input whenever they are
-- available, and only draws from the 'Y' input when 'X' would block.
wye :: Process a a' -> Process b b' -> Wye a' b' c -> Wye a b c
wye ma mb m = case m of
  Yield o k           -> Yield o (wye ma mb k)
  Stop                -> Stop
  Await f X ff        -> case ma of
    Yield a k           -> wye k mb (f a)
    Stop                -> wye Stop mb ff
    Await g h fg        -> Await (\a -> wye (g (h a)) mb m) X (wye fg mb m)
  Await f Y ff        -> case mb of
    Yield b k           -> wye ma k (f b)
    Stop                -> wye ma Stop ff
    Await g h fg     -> Await (\b -> wye ma (g (h b)) m) Y (wye ma fg m)
  Await f Z ff        -> case ma of
    Yield a k           -> wye k mb (f $ Left a)
    Stop                -> case mb of
      Yield b k           -> wye Stop k (f $ Right b)
      Stop                -> wye Stop Stop ff
      Await g h fg     -> Await (\b -> wye Stop (g (h b)) m) Y (wye Stop fg m)
    Await g pg fg     -> case mb of
      Yield b k         -> wye ma k (f $ Right b)
      Stop              -> Await (\a -> wye (g (pg a)) Stop m) X (wye fg Stop m)
      Await h ph fh     -> Await (\c -> case c of
                                                  Left a  -> wye (g (pg a)) mb m
                                                  Right b -> wye ma (h (ph b)) m) Z (wye fg fh m)

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
capX s t = fit (capped Right) (addX s t)
{-# INLINE capX #-}

-- | Tie off one input of a tee by connecting it to a known source.
capY :: Source b -> Wye a b c -> Process a c
capY s t = fit (capped Left) (addY s t)
{-# INLINE capY #-}

-- | Natural transformation used by 'capX' and 'capY'
capped :: (a -> Either a a) -> Y a a b -> a -> b
capped _ X = id
capped _ Y = id
capped f Z = f
{-# INLINE capped #-}
