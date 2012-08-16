{-# LANGUAGE Rank2Types, GADTs, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Transducer
-- Copyright   :  (C) 2012 Edward Kmett, Runar Bjarnason, Paul Chiusano
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
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
  , starve
  , transform

  -- ** Compilation
  , compile
  , before
  , repeatedly
  , sink

  -- * Pipes
  , Pipe
  , pipe
  , fun
  , feed
  , prepended
  , filtered
  , dropping
  , taking
  , droppingWhile
  , takingWhile
  , buffered

  -- * Sources
  , Source
  , source
  , cap

  -- * Tees
  , Fork(..)
  , tee
  , liftL, liftR
  , passL, passR
  , addL, addR
  , capL, capR
  ) where

import Control.Applicative
import Control.Category
import Control.Monad (ap, MonadPlus(..), replicateM_, when)
-- import Data.Bifunctor
import Data.Foldable
import Prelude hiding ((.),id)

-------------------------------------------------------------------------------
-- Stream Transduction Monad
-------------------------------------------------------------------------------

newtype SM k i o a = SM
  { runSM :: forall r.
      (a -> r) ->           -- ^ return
      (o -> r -> r) ->      -- ^ emit
      (k i r -> r -> r) ->  -- ^ receive
      r ->                  -- ^ fail
      r
  }

instance Functor (SM k i o) where
  fmap f (SM m) = SM $ \k -> m (k . f)

-- instance Bifunctor (SM k i) where
--  bimap f g (SM m) = SM $ \kp ke -> m (kp . g) (ke . f)

instance Applicative (SM k i o) where
  pure a = SM (\kp _ _ _ -> kp a)
  (<*>) = ap

instance Alternative (SM k i o) where
  empty = SM $ \_ _ _ kf -> kf
  SM m <|> SM n = SM $ \kp ke kr kf -> m kp ke (\kir _ -> kr kir (n kp ke kr kf)) kf

instance Monad (SM k i o) where
  return a = SM (\kp _ _ _ -> kp a)
  SM m >>= f = SM (\kp ke kr kf -> m (\a -> runSM (f a) kp ke kr kf) ke kr kf)
  fail _ = SM (\_ _ _ kf -> kf)

instance MonadPlus (SM k i o) where
  mzero = empty
  mplus = (<|>)

emit :: o -> SM k i o ()
emit o = SM (\kp ke _ _ -> ke o (kp ()))

receive :: SM (->) i o i
receive = SM (\kp _ kr kf -> kr kp kf)

receives :: Functor (k i) => k i j -> SM k i o j
receives ij = SM $ \kp _ kr kf -> kr (fmap kp ij) kf

stop :: SM k i o a
stop = empty

starve :: SF k a b -> [b]
starve Stop          = []
starve (Emit o k)    = o : starve k
starve (Receive _ _ f) = starve f

-------------------------------------------------------------------------------
-- Stream Transducers
-------------------------------------------------------------------------------

data SF k i o
  = Emit o (SF k i o)
  | forall r. Receive (r -> SF k i o) (k i r) (SF k i o)
  | Stop

instance Functor (SF k i) where
  fmap f (Emit o xs) = Emit (f o) (fmap f xs)
  fmap f (Receive k kir e) = Receive (fmap f . k) kir (fmap f e)
  fmap _ Stop = Stop

transform :: (forall a. k i a -> k' i' a) -> SF k i o -> SF k' i' o
transform f (Emit o k)    = Emit o (transform f k)
transform _ Stop          = Stop
transform f (Receive g kir h) = Receive (transform f . g) (f kir) (transform f h)

runSF :: Functor (k i) => (o -> r -> r) -> (k i r -> r -> r) -> r -> SF k i o -> r
runSF ke kr kf m = go m where
  go (Emit o k)        = ke o (go k)
  go (Receive f kir r) = kr (fmap (go . f) kir) (go r)
  go Stop              = kf
{-# INLINE runSF #-}

compile :: SM k i o a -> SF k i o
compile m = runSM m (const Stop) Emit (Receive id) Stop

repeatedly :: SM k i o a -> SF k i o
repeatedly m = r where r = runSM m (const r) Emit (Receive id) Stop

before :: SF k i o -> SM k i o a -> SF k i o
before f m = runSM m (const f) Emit (Receive id) Stop


instance Category (SF (->)) where
  id = repeatedly $ do
     i <- receive
     emit i

  Stop        . _                = Stop
  Emit a as   . sf               = Emit a (as . sf)
  Receive f kir _ . Emit b bs    = fmap f kir b . bs
  Receive _ _ k . Stop           = k . Stop
  sf          . Receive g kir fg = Receive (\a -> sf . g a) kir (sf . fg)

fun :: (a -> b) -> Pipe a b
fun f = repeatedly $ do
  i <- receive
  emit (f i)

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

taking :: Int -> Pipe a a
taking n = compile . replicateM_ n $ receive >>= emit

takingWhile :: (a -> Bool) -> Pipe a a
takingWhile p = repeatedly $ receive >>= \v -> if p v then emit v else stop

droppingWhile :: (a -> Bool) -> Pipe a a
droppingWhile p = before id loop where
  loop = receive >>= \v -> if p v then loop else emit v

-- | bolt a pipe on the end of any stream transducer
pipe :: Pipe b c -> SF k a b -> SF k a c
pipe Stop               _                 = Stop
pipe (Emit a as)       sf                 = Emit a (pipe as sf)
pipe (Receive f kir _) (Emit b bs)        = pipe (fmap f kir b) bs
pipe (Receive _ _ g)   Stop               = pipe g Stop
pipe sf                (Receive g kir fg) = Receive (fmap (pipe sf) g) kir (pipe sf fg)

buffered :: Int -> Pipe a [a]
buffered = repeatedly . go [] where
  go acc 0 = emit (reverse acc)
  go acc n = do
    i <- receive <|> emit (reverse acc) *> stop
    go (i:acc) $! n-1

feed :: [a] -> Pipe a b -> Pipe a b
feed []     r             = r
feed _      Stop          = Stop
feed (x:xs) (Receive f kir _) = feed xs (fmap f kir x)
feed xs     (Emit o k)    = Emit o (feed xs k)

-------------------------------------------------------------------------------
-- Source
-------------------------------------------------------------------------------

type Source b = forall k a. SF k a b

source :: Foldable f => f b -> Source b
source xs = compile (traverse_ emit xs)

-- | @'cap' = 'pipe'@
cap :: Pipe a b -> Source a -> Source b
cap l r = pipe l r

-------------------------------------------------------------------------------
-- Sink
-------------------------------------------------------------------------------

-- | \"Is that your final answer?\"
sink :: (forall o. SM k i o a) -> SF k i a
sink m = runSM m (\a -> Emit a Stop) id (Receive id) Stop

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
liftL = runSF Emit (Receive id . L) Stop

liftR :: Pipe b c -> Tee a b c
liftR = runSF Emit (Receive id . R) Stop

tee :: Pipe a a' -> Pipe b b' -> Tee a' b' c -> Tee a b c
tee a b (Emit c sf) = Emit c $ tee a b sf
tee _ _ Stop        = Stop
tee (Emit a sf) b (Receive f (L kf) _) = tee sf b (fmap f kf a)
tee Stop b (Receive _ L{} ff) = tee Stop b ff
tee (Receive g kg fg) b (Receive f (L kf) ff) = Receive id (L (\a -> tee (fmap g kg a) b (Receive f (L kf) ff))) (tee fg b (Receive f (L kf) ff))
tee a (Emit b sf) (Receive f (R kf) _) = tee a sf (fmap f kf b)
tee a Stop (Receive _ R{} ff) = tee a Stop ff
tee a (Receive g kg fg) (Receive f (R kf) ff) = Receive id (R (\b -> tee a (fmap g kg b) (Receive f (R kf) ff))) (tee a fg (Receive f (R kf) ff))

addL :: Pipe a b -> Tee b c d -> Tee a c d
addL p = tee p id

addR :: Pipe b c -> Tee a c d -> Tee a b d
addR = tee id

capL :: Source a -> Tee a b c -> Pipe b c
capL s t = transform capped (addL s t)

capR :: Source b -> Tee a b c -> Pipe a c
capR s t = transform capped (addR s t)

capped :: Fork (Either a a) b -> a -> b
capped (R r) = r
capped (L r) = r
