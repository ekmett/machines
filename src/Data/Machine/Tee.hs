{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
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
  , TG(..)
  , T
  , tee, teeT
  , addL, addR
  , capL, capR
  , zipWithT
  , zipWith
  , zipping
  ) where

import Data.Machine.Is
import Data.Machine.Plan
import Data.Machine.Process
import Data.Machine.Type
import Data.Machine.Source
import Prelude hiding ((.),id, zipWith)

-------------------------------------------------------------------------------
-- Tees
-------------------------------------------------------------------------------


data TG l r c where
  L :: l a -> TG l r a
  R :: r b -> TG l r b

-- | The input descriptor for a 'Tee' or 'TeeT'
type T a b = TG (Is a) (Is b)

-- | A 'Machine' that can read from two input stream in a deterministic manner.
type Tee a b c = Machine (T a b) c

-- | A 'Machine' that can read from two input stream in a deterministic manner with monadic side-effects.
type TeeT m a b c = MachineT m (T a b) c

-- type TeeGT m a b c = MachineT m (T a b) c

-- | Compose a pair of pipes onto the front of a Tee.
tee :: Monad m => ProcessT m a a' -> ProcessT m b b' -> TeeT m a' b' c -> TeeT m a b c
tee ma mb m = MachineT $ runMachineT m >>= \v -> case v of
  Stop         -> return Stop
  Yield o k    -> return $ Yield o $ tee ma mb k
  Await f (L Refl) ff -> runMachineT ma >>= \u -> case u of
    Stop            -> runMachineT $ tee stopped mb ff
    Yield a k       -> runMachineT $ tee k mb $ f a
    Await g Refl fg ->
      return $ Await (\a -> tee (g a) mb $ encased v) (L Refl) $ tee fg mb $ encased v
  Await f (R Refl) ff -> runMachineT mb >>= \u -> case u of
    Stop            -> runMachineT $ tee ma stopped ff
    Yield b k       -> runMachineT $ tee ma k $ f b
    Await g Refl fg ->
      return $ Await (\b -> tee ma (g b) $ encased v) (R Refl) $ tee ma fg $ encased v

-- | `teeT mt ma mb` Use a `Tee` to interleave or combine the outputs of `ma`
--   and `mb`
teeT :: Monad m => TeeT m a b c -> MachineT m k a -> MachineT m k b -> MachineT m k c
teeT mt ma mb = MachineT $ runMachineT mt >>= \v -> case v of
  Stop         -> return Stop
  Yield o k    -> return $ Yield o $ teeT k ma mb
  Await f (L Refl) ff -> runMachineT ma >>= \u -> case u of
    Stop          -> runMachineT $ teeT ff stopped mb
    Yield a k     -> runMachineT $ teeT (f a) k mb
    Await g rq fg ->
      return $ Await (\r -> teeT mt (g r) mb) rq $ teeT mt fg mb
  Await f (R Refl) ff -> runMachineT mb >>= \u -> case u of
    Stop          -> runMachineT $ teeT ff ma stopped
    Yield a k     -> runMachineT $ teeT (f a) ma k
    Await g rq fg ->
      return $ Await (\r -> teeT mt ma (g r)) rq $ teeT mt ma fg

-- | Precompose a pipe onto the left input of a tee.
addL :: Monad m => ProcessT m a b -> TeeT m b c d -> TeeT m a c d
addL p = tee p echo
{-# INLINE addL #-}

-- | Precompose a pipe onto the right input of a tee.
addR :: Monad m => ProcessT m b c -> TeeT m a c d -> TeeT m a b d
addR = tee echo
{-# INLINE addR #-}

-- | Tie off one input of a tee by connecting it to a known source.
capL :: Monad m => SourceT m a -> TeeT m a b c -> ProcessT m b c
capL s t = fit cappedT $ addL s t
{-# INLINE capL #-}

-- | Tie off one input of a tee by connecting it to a known source.
capR :: Monad m => SourceT m b -> TeeT m a b c -> ProcessT m a c
capR s t = fit cappedT $ addR s t
{-# INLINE capR #-}

-- | Natural transformation used by 'capL' and 'capR'.
cappedT :: T a a b -> Is a b
cappedT (R Refl) = Refl
cappedT (L Refl) = Refl
{-# INLINE cappedT #-}

-- | wait for both the left and the right sides of a T and then merge them with f.
zipWithT :: (a -> b -> c) -> PlanT (T a b) c m ()
zipWithT f = do { a <- awaits (L Refl); b <- awaits (R Refl); yield $ f a b }
{-# INLINE zipWithT #-}

-- | Zip together two inputs, then apply the given function,
--   halting as soon as either input is exhausted.
--   This implementation reads from the left, then the right
zipWith :: (a -> b -> c) -> Tee a b c
zipWith f = repeatedly $ do
  a <- awaits $ L Refl
  b <- awaits $ R Refl
  yield (f a b)
{-# INLINE zipWith #-}

-- | Zip together two inputs, halting as soon as either input is exhausted.
zipping :: Tee a b (a, b)
zipping = zipWith (,)
{-# INLINE zipping #-}
