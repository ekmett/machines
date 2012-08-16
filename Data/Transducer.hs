{-# LANGUAGE PolyKinds, ConstraintKinds, Rank2Types, GADTs, FlexibleInstances #-}
module Data.Transducer
  (
  -- * A monad for stream transduction
    SM(..)
  , emit
  , receive
  , receives

  -- * Stream Transducers
  , SF(..)
  , runSF
  , repeated
  , before
  , repeatedly

  -- * Pipes
  , Pipe
  , pipe
  , prepended
  , filtered
  , dropping
  -- * Tees
  , Fork(..)
  , tee
  , liftL, liftR
  , passL, passR
  , addL, addR
  ) where

import Control.Applicative
import Control.Category
import Control.Monad (ap)
import Control.Monad
import Data.Bifunctor
import Data.Foldable
import Prelude hiding ((.),id)

-------------------------------------------------------------------------------
-- Stream Transduction Monad
-------------------------------------------------------------------------------

newtype SM k i o a = SM { runSM :: forall r. (a -> r) -> (o -> r -> r) -> (k i r -> r) -> r }

instance Functor (SM k i o) where
  fmap f (SM m) = SM $ \k -> m (k . f)

instance Bifunctor (SM k i) where
  bimap f g (SM m) = SM $ \kp ke -> m (kp . g) (ke . f)

instance Applicative (SM k i o) where
  pure a = SM (\kp _ _ -> kp a)
  (<*>) = ap

instance Monad (SM k i o) where
  return a = SM (\kp _ _ -> kp a)
  SM m >>= f = SM (\kp ke kr -> m (\a -> runSM (f a) kp ke kr) ke kr)

emit :: o -> SM k i o ()
emit o = SM (\kp ke _ -> ke o (kp ()))

receive :: SM (->) i o i
receive = SM (\kp _ kr -> kr kp)

receives :: Functor (k i) => k i j -> SM k i o j
receives ij = SM $ \kp _ kr -> kr $ fmap kp ij

-------------------------------------------------------------------------------
-- Stream Transduction Category
-------------------------------------------------------------------------------

data SF k i o
  = Emit o (SF k i o)
  | Receive (k i (SF k i o))

runSF :: Functor (k i) => (o -> r -> r) -> (k i r -> r) -> SF k i o -> r
runSF ke kr m = go m where
  go (Emit o k) = ke o (go k)
  go (Receive f) = kr (fmap go f)
{-# INLINE runSF #-}

repeatedly :: SM k i o a -> SF k i o
repeatedly m = r where r = runSM m (const r) Emit Receive

before :: SF k i o -> SM k i o a -> SF k i o
before f m = runSM m (const f) Emit Receive

instance Category (SF (->)) where
  id = repeatedly $ do
     i <- receive
     emit i
  Emit a as . sf = Emit a (as . sf)
  Receive f . Emit b bs = f b . bs
  sf . Receive g = Receive (\a -> sf . g a)

repeated :: o -> SF k i o
repeated = repeatedly . emit

type Pipe = SF (->)

prepended :: Foldable f => f a -> Pipe a a
prepended = before id . traverse_ emit

filtered :: (a -> Bool) -> Pipe a a
filtered p = repeatedly $ do
  i <- receive
  when (p i) $ emit i

-- | a pipe that drops the first n entries it receives
dropping :: Int -> Pipe a a
dropping n = before id $ replicateM_ n receive

-- | bolt a pipe on the end of any stream transducer
pipe :: Functor (k a) => Pipe b c -> SF k a b -> SF k a c
pipe (Emit a as) sf = Emit a (pipe as sf)
pipe (Receive f) (Emit b bs) = pipe (f b) bs
pipe sf          (Receive g) = Receive (fmap (pipe sf) g)

buffered :: Int -> Pipe a [a]
buffered n = repeatedly $ do
  xs <- replicateM n receive
  emit xs
-------------------------------------------------------------------------------
-- Tees
-------------------------------------------------------------------------------

data Fork ab c where
  L :: (a -> c) -> Fork (Either a b) c
  R :: (b -> c) -> Fork (Either a b) c

instance Functor (Fork ab) where
  fmap f (L k) = L (f . k)
  fmap f (R k) = R (f . k)

type Tee a b = SF Fork (Either a b)

passL :: Tee a b a
passL = repeatedly $ do
  a <- receives (L id)
  emit a

passR :: Tee a b b
passR = repeatedly $ do
  b <- receives (R id)
  emit b

liftL :: Pipe a c -> Tee a b c
liftL = runSF Emit (Receive . L)

liftR :: Pipe b c -> Tee a b c
liftR = runSF Emit (Receive . R)

tee :: Pipe a a' -> Pipe b b' -> Tee a' b' c -> Tee a b c
tee a b (Emit c sf) = Emit c $ tee a b sf
tee (Emit a sf) b (Receive (L f)) = tee sf b (f a)
tee (Receive g) b (Receive (L f)) = Receive (L (\a -> tee (g a) b (Receive (L f))))
tee a (Emit b sf) (Receive (R f)) = tee a sf (f b)
tee a (Receive g) (Receive (R f)) = Receive (R (\b -> tee a (g b) (Receive (R f))))

addL :: Pipe a b -> Tee b c d -> Tee a c d
addL p = tee p id

addR :: Pipe b c -> Tee a c d -> Tee a b d
addR = tee id

