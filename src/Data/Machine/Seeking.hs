{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Data.Machine.Seeking where

import Data.Machine.Is
import Data.Machine.Tee
import Data.Machine.Type

data Seek t a c where
  Pure :: Seek t a a
  Seek :: t -> Seek t a ()

-- | A 'Machine' that sends "seeking" instructions upstream.
type Seeking t a b = Machine (Seek t a) b

-- | A 'Machine' that sends "seeking" instructions upstream with monadic side-effects.
type SeekingT m t a b = MachineT m (Seek t a) b

seeks :: (Monad m) => (t -> MachineT m a b) -> SeekingT m t b c -> t -> MachineT m a c
seeks fm m t0 = go (fm t0) m
  where
    go ma mb = MachineT $ runMachineT mb >>= \v -> case v of
      Stop -> return Stop
      Yield o k -> return $ Yield o $ go ma k
      Await f Pure ff -> runMachineT ma >>= \u -> case u of
        Stop -> runMachineT $ go stopped ff
        Yield a k -> runMachineT $ go k (f a)
        Await g rq fg ->
          return $ Await (\r -> go (g r) (encased v)) rq $ go fg (encased v)
      Await f (Seek t) _ -> runMachineT $ seeks fm (f ()) t

traceSeekL :: (Monad m) => TeeGT m (Is (Maybe t)) b c -> SeekingT m t c d -> MachineT m b d
traceSeekL ma mb = MachineT $ runMachineT mb >>= \v -> case v of
      Stop -> return Stop
      Yield o k -> return $ Yield o $ traceSeekL ma k
      Await f Pure ff -> runMachineT ma >>= \u -> case u of
        Stop -> runMachineT $ traceSeekL stopped ff
        Yield a k -> runMachineT $ traceSeekL k (f a)
        Await g (L Refl) _ -> runMachineT $ traceSeekL (g Nothing) (encased v)
        Await g (R rq) fg -> return $ Await (\r -> traceSeekL (g r) (encased v)) rq $ traceSeekL fg (encased v)
      Await f (Seek t) ff -> go ma
        where
          go m = runMachineT m >>= \u -> case u of
            Stop -> runMachineT $ traceSeekL stopped ff
            Yield _ k -> go k
            Await g (L Refl) _ -> runMachineT $ traceSeekL (g $ Just t) (f ())
            Await g (R rq) fg -> return $ Await (\r -> MachineT $ go (g r)) rq $ MachineT $ go fg

traceSeekR :: (Monad m) => TeeGT m a (Is (Maybe t)) c -> SeekingT m t c d -> MachineT m a d
traceSeekR ma mb = MachineT $ runMachineT mb >>= \v -> case v of
      Stop -> return Stop
      Yield o k -> return $ Yield o $ traceSeekR ma k
      Await f Pure ff -> runMachineT ma >>= \u -> case u of
        Stop -> runMachineT $ traceSeekR stopped ff
        Yield a k -> runMachineT $ traceSeekR k (f a)
        Await g (R Refl) _ -> runMachineT $ traceSeekR (g Nothing) (encased v)
        Await g (L rq) fg -> return $ Await (\r -> traceSeekR (g r) (encased v)) rq $ traceSeekR fg (encased v)
      Await f (Seek t) ff -> go ma
        where
          go m = runMachineT m >>= \u -> case u of
            Stop -> runMachineT $ traceSeekR stopped ff
            Yield _ k -> go k
            Await g (R Refl) _ -> runMachineT $ traceSeekR (g $ Just t) (f ())
            Await g (L rq) fg -> return $ Await (\r -> MachineT $ go (g r)) rq $ MachineT $ go fg
