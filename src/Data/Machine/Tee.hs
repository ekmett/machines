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
    Tee, TeeT, TeeG, TeeGT
  , TG(..)
  , T
  , el, ar
  , tee, teeT
  , addL, addR
  , capL, capR
  , zipWithT
  , zipWith
  , zipping
  , assoc
  , unassoc
  , flipTee
  ) where

import Control.Category
import Data.Machine.Is
import Data.Machine.Plan
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

type TeeG a b c = Machine (TG a b) c

type TeeGT m a b c = MachineT m (TG a b) c

-- | A 'Machine' that can read from two input stream in a deterministic manner.
type Tee a b c = Machine (T a b) c

-- | A 'Machine' that can read from two input stream in a deterministic manner with monadic side-effects.
type TeeT m a b c = MachineT m (T a b) c

el :: Category k => TG (k a) b a
el = L id

ar :: Category k => TG a (k b) b
ar = R id

-- | Compose a pair of pipes onto the front of a Tee.
tee :: Monad m => MachineT m a a' -> MachineT m b b' -> TeeT m a' b' c -> TeeGT m a b c
tee ma mb m = MachineT $ runMachineT m >>= \v -> case v of
  Stop         -> return Stop
  Yield o k    -> return $ Yield o $ tee ma mb k
  Await f (L Refl) ff -> runMachineT ma >>= \u -> case u of
    Stop            -> runMachineT $ tee stopped mb ff
    Yield a k       -> runMachineT $ tee k mb $ f a
    Await g rq fg ->
      return $ Await (\a -> tee (g a) mb $ encased v) (L rq) $ tee fg mb $ encased v
  Await f (R Refl) ff -> runMachineT mb >>= \u -> case u of
    Stop            -> runMachineT $ tee ma stopped ff
    Yield b k       -> runMachineT $ tee ma k $ f b
    Await g rq fg ->
      return $ Await (\b -> tee ma (g b) $ encased v) (R rq) $ tee ma fg $ encased v

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
addL :: Monad m => MachineT m a b -> TeeGT m (Is b) c d -> TeeGT m a c d
addL p m = MachineT $ runMachineT m >>= \v -> case v of
  Stop         -> return Stop
  Yield o k    -> return $ Yield o $ addL p k
  Await f (L Refl) ff -> runMachineT p >>= \u -> case u of
    Stop            -> runMachineT $ addL stopped ff
    Yield a k       -> runMachineT $ addL k $ f a
    Await g rq fg ->
      return $ Await (\a -> addL (g a) $ encased v) (L rq) $ addL fg $ encased v
  Await f (R rq) ff -> return $ Await (\r -> addL p (f r)) (R rq) $ addL p ff

-- | Precompose a pipe onto the right input of a tee.
addR :: Monad m => MachineT m b c -> TeeGT m a (Is c) d -> TeeGT m a b d
addR p m = MachineT $ runMachineT m >>= \v -> case v of
  Stop         -> return Stop
  Yield o k    -> return $ Yield o $ addR p k
  Await f (L rq) ff -> return $ Await (\l -> addR p (f l)) (L rq) $ addR p ff
  Await f (R Refl) ff -> runMachineT p >>= \u -> case u of
    Stop            -> runMachineT $ addR stopped ff
    Yield b k       -> runMachineT $ addR k $ f b
    Await g rq fg ->
      return $ Await (\b -> addR (g b) $ encased v) (R rq) $ addR fg $ encased v

-- | Tie off one input of a tee by connecting it to a known source.
capL :: Monad m => SourceT m a -> TeeGT m (Is a) b c -> MachineT m b c
capL s t = fit cappedT $ addL s t
{-# INLINE capL #-}

-- | Tie off one input of a tee by connecting it to a known source.
capR :: Monad m => SourceT m b -> TeeGT m a (Is b) c -> MachineT m a c
capR s t = fit cappedT $ addR s t
{-# INLINE capR #-}

-- | Natural transformation used by 'capL' and 'capR'.
cappedT :: TG a a b -> a b
cappedT (R rq) = rq
cappedT (L rq) = rq
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

assoc :: Monad m => TeeGT m (TG a b) c d -> TeeGT m a (TG b c) d
assoc m = MachineT $ runMachineT m >>= \v -> case v of
  Stop -> return Stop
  Yield o k -> return $ Yield o $ assoc k
  Await f (L (L rq)) ff -> return $ Await (\l -> assoc $ f l) (L rq) $ assoc ff
  Await f (L (R rq)) ff -> return $ Await (\l -> assoc $ f l) (R $ L rq) $ assoc ff
  Await f (R rq) ff -> return $ Await (\l -> assoc $ f l) (R $ R rq) $ assoc ff

unassoc :: Monad m => TeeGT m a (TG b c) d -> TeeGT m (TG a b) c d
unassoc m = MachineT $ runMachineT m >>= \v -> case v of
  Stop -> return Stop
  Yield o k -> return $ Yield o $ unassoc k
  Await f (L rq) ff -> return $ Await (\l -> unassoc $ f l) (L $ L rq) $ unassoc ff
  Await f (R (L rq)) ff -> return $ Await (\l -> unassoc $ f l) (L $ R rq) $ unassoc ff
  Await f (R (R rq)) ff -> return $ Await (\l -> unassoc $ f l) (R rq) $ unassoc ff

flipTee :: Monad m => TeeGT m a b d -> TeeGT m b a d
flipTee m = MachineT $ runMachineT m >>= \v -> case v of
  Stop -> return Stop
  Yield o k -> return $ Yield o $ flipTee k
  Await f (L rq) ff -> return $ Await (\l -> flipTee $ f l) (R rq) $ flipTee ff
  Await f (R rq) ff -> return $ Await (\l -> flipTee $ f l) (L rq) $ flipTee ff
