{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    Tee, TeeT
  , T(..)
  , tee, teeT, tee'
  , addL, addR
  , capL, capR, capT
  , zipWithT
  , zipWith
  , zipping
  ) where

import Control.Category
import Data.Machine.Is
import Data.Machine.Plan
import Data.Machine.Process
import Data.Machine.Type
import Data.Machine.Source
import Prelude hiding ((.), id, zipWith)

-------------------------------------------------------------------------------
-- Tees
-------------------------------------------------------------------------------

-- | The input descriptor for a 'Tee' or 'TeeT'
data T (a :: * -> *) (b :: * -> *) c where
  L :: a t -> T a b t
  R :: b t -> T a b t

-- | A 'Machine' that can read from two input stream in a deterministic manner.
type Tee a b c = Translate c (T a b)

-- | A 'Machine' that can read from two input stream in a deterministic manner with monadic side-effects.
type TeeT m a b c = TranslateT m c (T a b)

-- | Compose a pair of pipes onto the front of a Tee.
--
-- Examples:
--
-- >>> import Data.Machine.Source
-- >>> run $ tee (source [1..]) (source ['a'..'c']) zipping
-- [(1,'a'),(2,'b'),(3,'c')]
--
tee :: Monad m => TranslateT m a' a -> TranslateT m b' b -> TeeT m a' b' c -> TeeT m a b c
tee ma mb m = tee' ma mb . m
{-# INLINE tee #-}

tee' :: forall m a' a b' b. Monad m => TranslateT m a' a -> TranslateT m b' b -> TranslateT m (T a' b') (T a b)
tee' ma mb = TranslateT $ \req -> case req of
    L reql -> runTranslateT ma reql >>= gol
    R reqr -> runTranslateT mb reqr >>= gor
  where
    gol :: forall t. Step m a' a t -> MStep m (T a' b') (T a b) t
    gol u = case u of
      Stop            -> return Stop
      Yield a k       -> return $ Yield a (tee' k mb)
      Await g reql2 fg -> return $ Await (\a -> gol =<< (g a)) (L reql2) (gol =<< fg)
    gor :: forall t. Step m b' b t -> MStep m (T a' b') (T a b) t
    gor u = case u of
      Stop            -> return Stop
      Yield a k       -> return $ Yield a (tee' ma k)
      Await g reqr2 fg -> return $ Await (\a -> gor =<< (g a)) (R reqr2) (gor =<< fg)
{-# INLINABLE tee' #-}

-- | `teeT mt ma mb` Use a `Tee` to interleave or combine the outputs of `ma`
--   and `mb`.
--
--   The resulting machine will draw from a single source.
--
-- Examples:
--
-- >>> import Data.Machine.Source
-- >>> run $ teeT zipping echo echo <~ source [1..5]
-- [(1,2),(3,4)]
--
teeT' :: forall m a k b. Monad m => TranslateT m a k-> TranslateT m b k -> TranslateT m (T a b) k
teeT' ma mb = TranslateT $ \req -> case req of
    L reql -> runTranslateT ma reql >>= gol
    R reqr -> runTranslateT mb reqr >>= gor
  where
    gol :: forall t. Step m a k t -> MStep m (T a b) k t    
    gol u = case u of
      Stop            -> return Stop
      Yield a k       -> return $ Yield a (teeT' k mb)
      Await g reql2 fg -> return $ Await (\a -> gol =<< (g a)) reql2 (gol =<< fg)
    gor :: forall t. Step m b k t -> MStep m (T a b) k t    
    gor u = case u of
      Stop            -> return Stop
      Yield a k       -> return $ Yield a (teeT' ma k)
      Await g reqr2 fg -> return $ Await (\a -> gor =<< (g a)) reqr2 (gor =<< fg)

teeT :: Monad m => TeeT m a b c -> TranslateT m a k -> TranslateT m b k -> TranslateT m c k
teeT mt ma mb = teeT' ma mb . mt

-- | Precompose a pipe onto the left input of a tee.
addL :: Monad m => TranslateT m a b -> TranslateT m (T a c) (T b c)
addL p = tee' p id
{-# INLINE addL #-}

-- | Precompose a pipe onto the right input of a tee.
addR :: Monad m => TranslateT m b c -> TranslateT m (T a b) (T a c)
addR = tee' id
{-# INLINE addR #-}

-- | Tie off one input of a tee by connecting it to a known source.
capL :: Monad m => SourceT m a -> TranslateT m (T a b) b
capL s = fit cappedT $ addL s
{-# INLINE capL #-}

-- | Tie off one input of a tee by connecting it to a known source.
capR :: Monad m => SourceT m b -> TranslateT m (T a b) a
capR s = fit cappedT $ addR s
{-# INLINE capR #-}

-- | Tie off both inputs to a tee by connecting them to known sources.
--   This is recommended over capping each side separately, as it is
--   far more efficient.
capT :: Monad m => SourceT m a -> SourceT m b -> SourceT m (T a b)
capT l r = plug $ tee' l r
{-# INLINE capT #-}

-- | Natural transformation used by 'capL' and 'capR'.
cappedT :: T a a t -> a t
cappedT (R req) = req
cappedT (L req) = req
{-# INLINE cappedT #-}

-- | wait for both the left and the right sides of a T and then merge them with f.
zipWithT :: (a -> b -> c) -> PlanT (T (Is a) (Is b)) c m ()
zipWithT f = do { a <- awaits (L id); b <- awaits (R id); yield $ f a b }
{-# INLINE zipWithT #-}

-- | Zip together two inputs, then apply the given function,
--   halting as soon as either input is exhausted.
--   This implementation reads from the left, then the right
zipWith :: (a -> b -> c) -> Tee (Is a) (Is b) (Is c)
zipWith f = repeatedly $ do
  a <- awaits $ L id
  b <- awaits $ R id
  yield (f a b)
{-# INLINE zipWith #-}

-- | Zip together two inputs, halting as soon as either input is exhausted.
zipping :: Tee (Is a) (Is b) (Is (a, b))
zipping = zipWith (,)
{-# INLINE zipping #-}
