{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    Wye, WyeT
  , Y(..)
  , wye
  , addX, addY
  , capX, capY, capWye
  ) where

import Control.Category
import Control.Monad
import Data.Machine.Process
import Data.Machine.Type
import Data.Machine.Is
import Data.Machine.Source
import Prelude hiding ((.),id)

-------------------------------------------------------------------------------
-- Wyes
-------------------------------------------------------------------------------

-- | The input descriptor for a 'Wye' or 'WyeT'
data Y a b c where
  X :: a t -> Y a b t            -- block waiting on the left input
  Y :: b r -> Y a b r            -- block waiting on the right input
  Z :: a t -> b r -> Y a b (Either t r) -- block waiting on either input

-- | A 'Machine' that can read from two input stream in a non-deterministic manner.
type Wye a b c = Translate (Y a b) c

-- | A 'Machine' that can read from two input stream in a non-deterministic manner with monadic side-effects.
type WyeT m a b c = TranslateT m (Y a b) c

-- | Compose a pair of pipes onto the front of a 'Wye'.

-- | Precompose a 'Process' onto each input of a 'Wye' (or 'WyeT').
--
-- This is left biased in that it tries to draw values from the 'X' input whenever they are
-- available, and only draws from the 'Y' input when 'X' would block.
wye :: forall m a' a b' b. Monad m => TranslateT m a' a -> TranslateT m b' b -> TranslateT m (Y a' b') (Y a b)
wye ma mb = TranslateT $ \req -> case req of
  X reqx -> runTranslateT ma reqx >>= gox mb
  Y reqy -> runTranslateT mb reqy >>= goy ma
  Z reqx reqy -> join $ goz <$> runTranslateT ma reqx <*> runTranslateT mb reqy
  where
    gox :: forall t. TranslateT m b' b -> Step m a' a t -> MStep m (Y a' b') (Y a b) t
    gox my stp = case stp of
      Stop -> return Stop
      Yield a k -> return $ Yield a (wye k my)
      Await g reqx2 fg -> return $ Await (gox my <=< g) (X reqx2) (gox my =<< fg)
    goy :: forall r. TranslateT m a' a -> Step m b' b r -> MStep m (Y a' b') (Y a b) r
    goy mx stp = case stp of
      Stop -> return Stop
      Yield b k -> return $ Yield b (wye mx k)
      Await g reqy2 fg -> return $ Await (goy mx <=< g) (Y reqy2) (goy mx =<< fg)
    goz :: Step m a' a t -> Step m b' b r -> MStep m (Y a' b') (Y a b) (Either t r)
    goz stp = case stp of
      Stop -> \stp2 -> case stp2 of
        Stop -> return Stop
        Yield b k -> return $ Yield (Right b) (wye stopped k)
        Await h reqy fh -> return $ Await (\i -> h i >>= fmap (fmap Right) . goy stopped) (Y reqy) (fh >>= fmap (fmap Right) . goy stopped)
      Yield a k -> const $ return $ Yield (Left a) (wye k mb)
      Await g reqx fg -> \stp2 -> case stp2 of
        Stop -> fmap (fmap Left) . gox stopped $ Await g reqx fg
        Yield b k -> return $ Yield (Right b) (wye ma k) -- This can cause repeated side effects from the left machine
        Await h reqy2 fh -> return $
          Await
            (\i -> case i of Left a -> flip goz stp2 =<< g a; Right b -> goz stp =<< h b)
            (Z reqx reqy2)
            (join $ goz <$> fg <*> fh)

-- | Precompose a pipe onto the left input of a wye.
addX :: Monad m => TranslateT m a b -> TranslateT m (Y a c) (Y b c)
addX p = wye p id
{-# INLINE addX #-}

-- | Precompose a pipe onto the right input of a wye.
addY :: Monad m => TranslateT m b c -> TranslateT m (Y a b) (Y a c)
addY = wye id
{-# INLINE addY #-}

-- | Stop off one input of a wye.
capX :: Translate (Y a b) b
capX = TranslateT $ \req -> case req of
    X _ -> return Stop
    Y reqy -> return $ Await (\i -> return $ Yield i capX) reqy (return Stop)
    Z _ reqy -> return $ Await (\i -> return $ Yield (Right i) capX) reqy (return Stop)
{-# INLINE capX #-}

-- | Tie off one input of a wye by connecting it to a known source.
capY :: Translate (Y a b) a
capY = TranslateT $ \req -> case req of
    Y _ -> return Stop
    X reqx -> return $ Await (\i -> return $ Yield i capY) reqx (return Stop)
    Z reqx _ -> return $ Await (\i -> return $ Yield (Left i) capY) reqx (return Stop)
{-# INLINE capY #-}

-- | Tie off both inputs of a wye by connecting them to known sources.
capWye :: Monad m => SourceT m a -> SourceT m b -> SourceT m (Y a b)
capWye a b = plug $ wye a b
{-# INLINE capWye #-}
