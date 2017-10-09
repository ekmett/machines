{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Type
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  rank-2, GADTs
--
----------------------------------------------------------------------------
module Data.Machine.Type
  (
  -- * Translate
    TranslateT(..)
  , Step(..)
  , MStep
  , Translate
  
  -- * Machines
  , MachineT(..)
  , Machine
  , machineT
  , runMachineT
  , runT_
  , runT
  , run
  , runMachine
  , encased

  -- ** Building machines from plans
  , construct
  , repeatedly
  , unfoldPlan
  , before
  , preplan
--  , sink

  -- ** Deconstructing machines back into plans
  , deconstruct
  , tagDone
  , finishWith

  -- * Reshaping machines
  , fit
  , fitM
  , pass

  , starve

  , stopped

  , stepMachine

  -- * Applicative Machines
  , Appliance(..)
  ) where

import Control.Applicative
import Control.Category
import Control.Monad (liftM, (<=<))
import Data.Foldable
import Data.Functor.Identity
import Data.Machine.Is
import Data.Machine.Plan
import Data.Monoid hiding ((<>))
import Data.Pointed
import Data.Profunctor.Unsafe ((#.))
import Data.Semigroup
import Prelude hiding ((.),id)

import Unsafe.Coerce

newtype TranslateT m u k =
  TranslateT { runTranslateT :: forall t. u t -> MStep m u k t}

type Translate u k = forall m. Monad m => TranslateT m u k

data Step m u k t where
  Stop :: Step m u k t
  Yield :: t -> TranslateT m u k -> Step m u k t
  Await :: (t' -> MStep m u k t) -> (k t') -> MStep m u k t -> Step m u k t

type MStep m u k t = m (Step m u k t)

type MachineT m k o = TranslateT m (Is o) k

instance (Monad m) => Category (TranslateT m) where
    id :: TranslateT m u u
    id = TranslateT $ \req -> return $ Await (\t -> return (Yield t id)) req (return Stop)
    {-# INLINABLE id #-}

    (.) :: forall u v k. TranslateT m u v -> TranslateT m k u -> TranslateT m k v
    t2 . t1 = TranslateT $ \req -> runTranslateT t1 req >>= go t2
      where
        go :: forall t. TranslateT m u v -> Step m k u t -> MStep m k v t
        go r2 s1 = case s1 of
            Stop -> return Stop
            Yield t r1 -> return $ Yield t $ r2 . r1
            Await g kg fg -> runTranslateT r2 kg >>= go2 g fg
        go2 :: (t' -> MStep m k u t) -> MStep m k u t -> Step m u v t' -> MStep m k v t
        go2 g fg s2 = case s2 of
            Stop -> go stopped =<< fg
            Yield t r -> go r =<< g t
            Await h kh fh -> return $ Await (\t -> go2 g fg =<< h t) kh (go2 g fg =<< fh)
    {-# INLINABLE (.) #-}

-------------------------------------------------------------------------------
-- Transduction Machines
-------------------------------------------------------------------------------

-- | This is the base functor for a 'Machine' or 'MachineT'.
--
-- Note: A 'Machine' is usually constructed from 'Plan', so it does not need to be CPS'd.
-- data Step k o r
--   = Stop
--   | Yield o r
--   | forall t. Await (t -> r) (k t) r

instance (Functor m) => Functor (Step m u k) where
  fmap _ Stop = Stop
  fmap f (Yield o k) = Yield (f o) k
  fmap f (Await g kg fg) = Await (fmap (fmap f) . g) kg (fmap (fmap f) fg)

-- -- | A 'MachineT' reads from a number of inputs and may yield results before stopping
-- -- with monadic side-effects.
-- newtype MachineT m k o = MachineT { runMachineT :: m (Step k o (MachineT m k o)) }

-- | A 'Machine' reads from a number of inputs and may yield results before stopping.
--
-- A 'Machine' can be used as a @'MachineT' m@ for any @'Monad' m@.
type Machine k o = forall m. Monad m => MachineT m k o

machineT :: (Functor m) => MStep m (Is o) k o -> MachineT m k o
machineT m = TranslateT $ \Refl -> m
{-# INLINE machineT #-}

runMachineT :: (Functor m) => MachineT m k o -> MStep m (Is o) k o
runMachineT m = runTranslateT m Refl
{-# INLINE runMachineT #-}

-- | @'runMachine' = 'runIdentity' . 'runMachineT'@
runMachine :: MachineT Identity k o -> Step Identity (Is o) k o
runMachine = runIdentity . runMachineT
{-# INLINE runMachine#-}

-- | Pack a 'Step' of a 'Machine' into a 'Machine'.
encased :: forall m o k. Applicative m => Step m (Is o) k o -> TranslateT m (Is o) k
encased s = machineT (pure s)

-- | Transform a 'Machine' by looking at a single step of that machine.
stepMachine ::
    Monad m => MachineT m k o ->
    (Step m (Is o) k o -> MachineT m k' o') -> MachineT m k' o'
stepMachine m f = machineT (runMachineT . f =<< runMachineT m)

fmapMachineT ::
    forall m k a b. (Functor m) => (a -> b) -> MachineT m k a -> MachineT m k b
fmapMachineT f m = machineT (fmap f' $ runMachineT m)
  where
    f' :: Step m (Is a) k a -> Step m (Is b) k b
    f' (Yield o (TranslateT xs)) = Yield (f o) $ machineT $
        fmap f' (xs Refl)
    f' (Await k kir e) = Await (fmap f' . k) kir (f' <$> e)
    f' Stop            = Stop

instance Monad m => Semigroup (TranslateT m u k) where
  TranslateT a <> b = TranslateT $ \req -> a req >>= f' req
    where
      f' :: forall t. u t -> Step m u k t -> MStep m u k t
      f' req step = case step of
        Yield o a'    -> return $ Yield o (a' <> b)
        Await k kir e -> return $
            Await (\x -> f' req =<< k x) kir (f' req =<< e)
        Stop          -> runTranslateT b req

instance (Monad m) => Monoid (TranslateT m u k) where
  mempty        = stopped
  mappend       = (<>)

-- | An input type that supports merging requests from multiple machines.
class Appliance k where
  applied :: Monad m => MachineT m k (a -> b) -> MachineT m k a -> MachineT m k b

{-
-- TODO

instance Appliance (Is i) where
  applied = appliedTo (Just mempty) (Just mempty) id (flip id) where

-- applied
appliedTo
  :: Maybe (Seq i)
  -> Maybe (i -> MachineT m (Is i) b, MachineT m (Is i) b)
  -> Either (Seq a) (Seq b)
  -> (a -> b -> c)
  -> (b -> a -> c)
  -> MachineT m (Is i) a
  -> MachineT m (Is i) b
  -> MachineT m (Is i) c
appliedTo mis blocking ss f g m n = MachineT $ runMachineT m >>= \v -> case v of
  Stop -> return Stop
  Yield a k -> case ss of
    Left as ->
    Right bs -> case viewl bs of
      b :< bs' -> return $ Yield (f a b) (appliedTo mis bs' f g m n)
      EmptyL   -> runMachine $ appliedTo mis blocking (singleton a) g f n m
  Await ak Refl e -> case mis of
    Nothing -> runMachine $ appliedTo Nothing blocking bs f g e n
    Just is -> case viewl is of
      i :< is' -> runMachine $ appliedTo (Just is') blocking bs f g (ak i) m
      EmptyL -> case blocking of
        Just (bk, be) ->
        Nothing -> runMachine $ appliedTo mis (Just (ak, e))
        | blocking  -> return $ Await (\i -> appliedTo (Just (singleton i)) False f g (ak i) n) Refl $
        | otherwise ->
-}

-- | Stop feeding input into model, taking only the effects.

runMStep_ :: (Monad m) => MStep m (Is o) k t -> m ()
runMStep_ ms = ms >>= \step -> case step of
  Stop -> return ()
  Yield _ k -> runT_ $ k
  Await _ _ e -> runMStep_ e
{-# INLINE runMStep_ #-}

runT_ :: (Monad m) => MachineT m u k -> m ()
runT_ m = runMStep_ $ runMachineT m
{-# INLINE runT_ #-}

runMStepT :: (Monad m) => MStep m (Is o) k o -> m [o]
runMStepT ms = ms >>= \step -> case step of
  Stop        -> return []
  Yield o k   -> liftM (o:) (runT k)
  Await _ _ e -> runMStepT e

-- | Stop feeding input into model and extract an answer
runT :: Monad m => MachineT m k b -> m [b]
runT m = runMStepT $ runMachineT m

-- | Run a pure machine and extract an answer.
run :: MachineT Identity k b -> [b]
run = runIdentity . runT

mapMachine :: forall m o p k. (Monad m) => (o -> p) -> MachineT m k o -> MachineT m k p
mapMachine f m = machineT $ runTranslateT m Refl >>= go
  where
    go :: Step m (Is o) k o -> MStep m (Is p) k p
    go v = case v of
      Yield o c -> return $ Yield (f o) (mapMachine f c)
      Stop -> return Stop
      Await g k fg -> return $ Await (go <=< g) k (go =<< fg)

-- | This permits toList to be used on a Machine.
foldMapStep :: (Monoid m, n ~ Identity) => (o -> m) -> MStep n (Is o) k o -> m
foldMapStep f m = go (runIdentity m) where
    go Stop = mempty
    go (Yield o k) = f o `mappend` foldMapMachine f k
    go (Await _ _ fg) = foldMapStep f fg

foldMapMachine :: (Monoid m, n ~ Identity) => (o -> m) -> MachineT n k o -> m
foldMapMachine f m = foldMapStep f (runTranslateT m Refl)

-- |
-- Connect different kinds of machines.
--
-- @'fit' 'id' = 'id'@
fit :: forall m k k' u. Monad m =>
    (forall a. k a -> k' a) -> TranslateT m u k -> TranslateT m u k'
fit f m = TranslateT $ \req -> liftM f' (runTranslateT m req) where
  f' :: Step m u k t -> Step m u k' t
  f' (Yield o k)     = Yield o (fit f k)
  f' Stop            = Stop
  f' (Await g kir h) = Await (fmap f' . g) (f kir) (fmap f' h)
{-# INLINE fit #-}

--- | Connect machine transformers over different monads using a monad
--- morphism.
fitM :: forall m m' k u. (Monad m, Monad m')
     => (forall a. m a -> m' a) -> TranslateT m u k -> TranslateT m' u k
fitM f m = TranslateT $ \req -> f (liftM aux $ runTranslateT m req)
  where
    aux :: Step m u k t -> Step m' u k t
    aux Stop = Stop
    aux (Yield o k) = Yield o (fitM f k)
    aux (Await g kg gg) = Await (f . fmap aux . g) kg (f $ fmap aux gg)
{-# INLINE fitM #-}

-- | Compile a machine to a model.
construct :: Monad m => PlanT k o m a -> MachineT m k o
construct m = machineT $ runPlanT m
  (const (return Stop))
  (\o k -> return (Yield o (machineT k)))
  (\f k g -> return (Await f k g))
  (return Stop)
{-# INLINE construct #-}

-- | Generates a model that runs a machine until it stops, then start it up again.
--
-- @'repeatedly' m = 'construct' ('Control.Monad.forever' m)@
repeatedly :: Monad m => PlanT k o m a -> MachineT m k o
repeatedly m = r where
  r = machineT $ runPlanT m
    (const (runMachineT r))
    (\o k -> return (Yield o (machineT k)))
    (\f k g -> return (Await f k g))
    (return Stop)
{-# INLINE repeatedly #-}

-- | Unfold a stateful PlanT into a MachineT.
unfoldPlan :: Monad m => s -> (s -> PlanT k o m s) -> MachineT m k o
unfoldPlan s0 sp = r s0 where
  r s = machineT $ runPlanT (sp s)
      (\sx -> runMachineT $ r sx)
      (\o k -> return (Yield o (machineT k)))
      (\f k g -> return (Await f k g))
      (return Stop)
{-# INLINE unfoldPlan #-}

-- | Evaluate a machine until it stops, and then yield answers according to the supplied model.
before :: Monad m => TranslateT m (Is o) k -> PlanT k o m a -> TranslateT m (Is o) k
before n m = machineT $ runPlanT m
  (const $ runTranslateT n Refl)
  (\o k -> return (Yield o (machineT k)))
  (\f k g -> return (Await f k g))
  (return Stop)
{-# INLINE before #-}

-- | Incorporate a 'Plan' into the resulting machine.
preplan :: Monad m => PlanT k o m (MachineT m k o) -> MachineT m k o
preplan m = machineT $ runPlanT m
  runMachineT
  (\o k -> return (Yield o (machineT k)))
  (\f k g -> return (Await f k g))
  (return Stop)
{-# INLINE preplan #-}

-- | Given a handle, ignore all other inputs and just stream input from that handle.
--
-- @
-- 'pass' 'id' :: 'Data.Machine.Process.Process' a a
-- 'pass' 'Data.Machine.Tee.L'  :: 'Data.Machine.Tee.Tee' a b a
-- 'pass' 'Data.Machine.Tee.R'  :: 'Data.Machine.Tee.Tee' a b b
-- 'pass' 'Data.Machine.Wye.X'  :: 'Data.Machine.Wye.Wye' a b a
-- 'pass' 'Data.Machine.Wye.Y'  :: 'Data.Machine.Wye.Wye' a b b
-- 'pass' 'Data.Machine.Wye.Z'  :: 'Data.Machine.Wye.Wye' a b (Either a b)
-- @
--
pass :: k o -> Machine k o
pass k = loop
  where
    loop = machineT $ return $
      Await (\t -> return (Yield t loop)) k (return Stop)
{-# INLINE pass #-}



-- | Run a machine with no input until it stops, then behave as another machine.
starve ::
    forall m k k0 u. Monad m => TranslateT m u k0 -> TranslateT m u k -> TranslateT m u k
starve m cont = TranslateT $ \req -> runTranslateT m req >>= go req
  where
    go :: forall t. u t -> Step m u k0 t -> MStep m u k t
    go req v = case v of
      Stop            -> runTranslateT cont req -- Continue with cont instead of stopping
      Yield o r       -> return $ Yield o (starve r cont)
      Await _ _ r     -> r >>= go req
{-# INLINE starve #-}

-- | This is a stopped 'Machine'
stopped :: forall m u k. (Applicative m) => TranslateT m u k
stopped = TranslateT $ \_ -> (pure Stop :: MStep m u k t)
{-# INLINE stopped #-}

--------------------------------------------------------------------------------
-- Deconstruction
--------------------------------------------------------------------------------

--- | Convert a 'Machine' back into a 'Plan'. The first value the
--- machine yields that is tagged with the 'Left' data constructor is
--- used as the return value of the resultant 'Plan'. Machine-yielded
--- values tagged with 'Right' are yielded -- sans tag -- by the
--- result 'Plan'. This may be used when monadic binding of results is
--- required.
deconstruct :: Monad m => MachineT m k (Either a o) -> PlanT k o m a
deconstruct t = go t 
  where
    go t = PlanT $ \r y a f ->
      let aux k = runPlanT (go k) r y a f
          aux2 v = case v of 
           Stop -> f
           Yield (Left o) _ -> r o
           Yield (Right o) k -> y o (aux k)
           Await g fk h -> a (aux2 <=< g) fk (aux2 =<< h)
      in runTranslateT t Refl >>= aux2

-- | Use a predicate to mark a yielded value as the terminal value of
-- this 'Machine'. This is useful in combination with 'deconstruct' to
-- combine 'Plan's.
tagDone :: Monad m => (o -> Bool) -> MachineT m k o -> MachineT m k (Either o o)
tagDone f = mapMachine aux
  where aux x = if f x then Left x else Right x

-- | Use a function to produce and mark a yielded value as the
-- terminal value of a 'Machine'. All yielded values for which the
-- given function returns 'Nothing' are yielded down the pipeline, but
-- the first value for which the function returns a 'Just' value will
-- be returned by a 'Plan' created via 'deconstruct'.
finishWith :: Monad m
           => (o -> Maybe r) -> MachineT m k o -> MachineT m k (Either r o)
finishWith f = mapMachine aux
  where aux x = maybe (Right x) Left $ f x


-------------------------------------------------------------------------------
-- Sink
-------------------------------------------------------------------------------

{-
-- |
-- A Sink in this model is a 'Data.Machine.Process.Process'
-- (or 'Data.Machine.Tee.Tee', etc) that produces a single answer.
--
-- \"Is that your final answer?\"
sink :: Monad m => (forall o. PlanT k o m a) -> MachineT m k a
sink m = runPlanT m (\a -> Yield a Stop) id (Await id) Stop
-}
