{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 0
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Process
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank 2 Types, GADTs
--
----------------------------------------------------------------------------
module Data.Machine.Process
  (
  -- * Processes
    Process
  , ProcessT
  , Automaton(..)
  , AutomatonM(..)
  , process
  -- ** Common Processes
  , (<~), (~>)
  , echo
  , supply
  , prepended
  , filtered
  , dropping
  , taking
  , droppingWhile
  , takingWhile
  , buffered
  , flattened
  , fold
  , fold1
  , scan
  , scan1
  , scanMap
  , asParts
  , sinkPart_
  , autoM
  , final
  , finalOr
  , intersperse
  , largest
  , smallest
  , sequencing
  , mapping
  , traversing
  , reading
  , showing
  , strippingPrefix
  ) where

import Control.Category
import Control.Arrow (Kleisli(..))
import Control.Monad (liftM)
import Data.Foldable hiding (fold)
import Data.Machine.Is
import Data.Machine.Plan
import Data.Machine.Type
import Data.Monoid
import Data.Void
import Prelude
#if !(MIN_VERSION_base(4,8,0))
  hiding (id, (.), foldr)
#else
  hiding (id, (.))
#endif

-- $setup
-- >>> import Data.Machine.Source

infixr 9 <~
infixl 9 ~>

-------------------------------------------------------------------------------
-- Processes
-------------------------------------------------------------------------------

-- | A @'Process' a b@ is a stream transducer that can consume values of type @a@
-- from its input, and produce values of type @b@ for its output.
type Process a b = Translate b (Is a)

-- | A @'ProcessT' m a b@ is a stream transducer that can consume values of type @a@
-- from its input, and produce values of type @b@ and has side-effects in the
-- 'Monad' @m@.
type ProcessT m a b = TranslateT m b (Is a)

-- | An 'Automaton' can be automatically lifted into a 'Process'
class Automaton k where
  auto :: k a b -> Process a (Is b)

instance Automaton (->) where
  auto = mapping

instance Automaton Is where
  auto Refl = echo

class AutomatonM x where
  autoT :: Monad m => x m a b -> ProcessT m a (Is b)

instance AutomatonM Kleisli where
  autoT (Kleisli k) = autoM k

-- | The trivial 'Process' that simply repeats each input it receives.
--
-- This can be constructed from a plan with
-- @
-- echo :: Process a a
-- echo = repeatedly $ do
--   i <- await
--   yield i
-- @
--
-- Examples:
--
-- >>> run $ echo <~ source [1..5]
-- [1,2,3,4,5]
--
echo :: Process a (Is a)
echo = TranslateT $ \Refl ->
    return $ Await (\t -> return (Yield t echo)) id (return Stop)
{-# INLINABLE echo #-}

-- | A 'Process' that prepends the elements of a 'Foldable' onto its input, then repeats its input from there.
prepended :: Foldable f => f a -> Process a (Is a)
prepended = before echo . traverse_ yield

-- | A 'Process' that only passes through inputs that match a predicate.
--
-- This can be constructed from a plan with
-- @
-- filtered :: (a -> Bool) -> Process a a
-- filtered p = repeatedly $ do
--   i <- await
--   when (p i) $ yield i
-- @
--
-- Examples:
--
-- >>> run $ filtered even <~ source [1..5]
-- [2,4]
--
filtered :: (a -> Bool) -> Process a (Is a)
filtered p =
    encased loop
  where
    loop = Await (\a -> if p a then pure (Yield a (encased loop)) else pure loop)
           id
           (pure Stop)
{-# INLINABLE filtered #-}

-- | A 'Process' that drops the first @n@, then repeats the rest.
--
-- This can be constructed from a plan with
-- @
-- dropping n = before echo $ replicateM_ n await
-- @
--
-- Examples:
--
-- >>> run $ dropping 3 <~ source [1..5]
-- [4,5]
--
dropping :: Int -> Translate k k
dropping n = TranslateT $ \req -> loop req n
  where
    loop req cnt
      | cnt <= 0
      = runTranslateT id req
      | otherwise
      = pure $ Await (\_ -> loop req (cnt - 1)) req (pure Stop)
{-# INLINABLE dropping #-}

-- | A 'Process' that passes through the first @n@ elements from its input then stops
--
-- This can be constructed from a plan with
-- @
-- taking n = construct . replicateM_ n $ await >>= yield
-- @
--
-- Examples:
--
-- >>> run $ taking 3 <~ source [1..5]
-- [1,2,3]
--
taking :: Int -> Translate k k
taking = loop
  where
    loop cnt = TranslateT $ \req -> pure $
      if cnt <= 0
        then Stop
        else Await (\v -> pure $ Yield v (loop (cnt - 1))) req (pure Stop)
{-# INLINABLE taking #-}

-- | A 'Process' that passes through elements until a predicate ceases to hold, then stops
--
-- This can be constructed from a plan with
-- @
-- takingWhile :: (a -> Bool) -> Process a a
-- takingWhile p = repeatedly $ await >>= \v -> if p v then yield v else stop
-- @
--
-- Examples:
--
-- >>> run $ takingWhile (\Refl x -> x < 3) <~ source [1..5 :: Int]
-- [1,2]
--
takingWhile :: (forall a. k a -> a -> Bool) -> Translate k k
takingWhile p =
    loop
  where
    loop = TranslateT $ \req -> pure $
      Await (\a -> pure $ if p req a then Yield a loop else Stop)
      req 
      (pure Stop)
{-# INLINABLE takingWhile #-}

-- | A 'Process' that drops elements while a predicate holds
--
-- This can be constructed from a plan with
-- @
-- droppingWhile :: (a -> Bool) -> Process a a
-- droppingWhile p = before echo loop where
--   loop = await >>= \v -> if p v then loop else yield v
-- @
--
-- Examples:
--
-- >>> run $ droppingWhile (\Refl x -> x < 3) <~ source [1..5 :: Int]
-- [3,4,5]
--
droppingWhile :: forall k m. (Monad m) => (forall a. k a -> a -> Bool) -> TranslateT m k k
droppingWhile p = TranslateT $ \req -> pure (loop req)
  where
    loop :: forall a. k a -> Step m k k a
    loop req = Await (\a -> pure $ if p req a then loop req else Yield a id)
           req
           (pure Stop)
{-# INLINABLE droppingWhile #-}

-- | Chunk up the input into `n` element lists.
--
-- Avoids returning empty lists and deals with the truncation of the final group.
--
-- An approximation of this can be constructed from a plan with
-- @
-- buffered :: Int -> Process a [a]
-- buffered = repeatedly . go [] where
--   go acc 0 = yield (reverse acc)
--   go acc n = do
--     i <- await <|> yield (reverse acc) *> stop
--     go (i:acc) $! n-1
-- @
--
-- Examples:
--
-- >>> run $ buffered 3 <~ source [1..6]
-- [[1,2,3],[4,5,6]]
--
-- >>> run $ buffered 3 <~ source [1..5]
-- [[1,2,3],[4,5]]
--
-- >>> run $ buffered 3 <~ source []
-- []
--
buffered :: forall m a. Monad m => Int -> ProcessT m a (Is [a])
buffered n =
    begin
  where
    -- The buffer is empty, if we don't get anything
    -- then we shouldn't yield at all.
    begin     = encased
              $ Await (\v -> loop (v:) (n - 1))
                      Refl
                      (pure Stop)

    -- The buffer (a diff list) contains elements, and
    -- we're at the requisite number, yield the
    -- buffer and restart
    loop :: ([a] -> [a]) -> Int -> MStep m (Is [a]) (Is a) [a]
    loop dl 0 = pure
              $ Yield (dl []) begin

    -- The buffer contains elements and we're not yet
    -- done, continue waiting, but if we don't receive
    -- anything, then yield what we have and stop.
    loop dl r = pure
              $ Await (\v -> loop (dl . (v:)) (r - 1))
                      Refl
                      (finish dl)

    -- All data has been retrieved, emit and stop.
    finish dl = pure
              $ Yield (dl []) stopped
{-# INLINABLE buffered #-}

-- | Build a new 'Machine' by adding a 'Process' to the output of an old 'Machine'
--
-- @
-- ('<~') :: 'Process' b c -> 'Process' a b -> 'Process' a c
-- ('<~') :: 'Process' c d -> 'Data.Machine.Tee.Tee' a b c -> 'Data.Machine.Tee.Tee' a b d
-- ('<~') :: 'Process' b c -> 'Machine' k b -> 'Machine' k c
-- @
(<~) :: Monad m => ProcessT m b c -> TranslateT m (Is b) k -> TranslateT m c k
(<~) = flip (.)
{-# INLINE (<~) #-}

-- | Flipped ('<~').
(~>) :: Monad m => TranslateT m (Is b) k -> ProcessT m b c -> TranslateT m c k
ma ~> mp = mp <~ ma
{-# INLINE (~>) #-}


-- | Feed a 'Process' some input.
--
-- Examples:
--
-- >>> run $ supply [1,2,3] echo <~ source [4..6]
-- [1,2,3,4,5,6]
--
supply :: forall f m a b . (Foldable f, Monad m) => f a -> ProcessT m a b -> ProcessT m a b
supply xs p = starve (foldr go stopped xs) id ~> p
  where
    go x m = encased (Yield x m)
{-# INLINABLE supply #-}

-- |
-- Convert a machine into a process, with a little bit of help.
--
-- @
-- choose :: 'Data.Machine.Tee.T' a b x -> (a, b) -> x
-- choose t = case t of
--   'Data.Machine.Tee.L' -> 'fst'
--   'Data.Machine.Tee.R' -> 'snd'
--
-- 'process' choose :: 'Data.Machine.Tee.Tee' a b c -> 'Data.Machine.Process.Process' (a, b) c
-- 'process' choose :: 'Data.Machine.Tee.Tee' a b c -> 'Data.Machine.Process.Process' (a, b) c
-- 'process' ('const' 'id') :: 'Data.Machine.Process.Process' a b -> 'Data.Machine.Process.Process' a b
-- @
process ::
    forall m o i k. Monad m =>
    (forall a. k a -> i -> a) -> TranslateT m o k -> ProcessT m i o
process f (TranslateT m) = TranslateT $ \req -> liftM f' (m req) where
  f' :: forall t. Step m o k t -> Step m o (Is i) t
  f' (Yield o k)     = Yield o (process f k)
  f' Stop            = Stop
  f' (Await g kir h) = Await (fmap f' . g . f kir) Refl (f' <$> h)

-- |
-- Construct a 'Process' from a left-scanning operation.
--
-- Like 'fold', but yielding intermediate values.
--
-- It may be useful to consider this alternative signature
-- @
-- 'scan' :: (a -> b -> a) -> a -> Process b a
-- @
--
-- For stateful 'scan' use 'auto' with "Data.Machine.Mealy" machine.
-- This can be constructed from a plan with
-- @
-- scan :: Category k => (a -> b -> a) -> a -> Machine (k b) a
-- scan func seed = construct $ go seed where
--   go cur = do
--     yield cur
--     next <- await
--     go $! func cur next
-- @
--
-- Examples:
--
-- >>> run $ scan (+) 0 <~ source [1..5]
-- [0,1,3,6,10,15]
--
-- >>> run $ scan (\a _ -> a + 1) 0 <~ source [1..5]
-- [0,1,2,3,4,5]
--
scan :: forall k a b. (Category k) => (a -> b -> a) -> a -> Translate (Is a) (k b)
scan func seed =
  let step t = t `seq` pure $ Yield t
             $ encased
             $ Await (step . func t)
                     id
                     (pure Stop)
  in  machineT (step seed)
{-# INLINABLE scan #-}

-- |
-- 'scan1' is a variant of 'scan' that has no starting value argument
--
-- This can be constructed from a plan with
-- @
-- scan1 :: Category k => (a -> a -> a) -> Machine (k a) a
-- scan1 func = construct $ await >>= go where
--   go cur = do
--     yield cur
--     next <- await
--     go $! func cur next
-- @
--
-- Examples:
--
-- >>> run $ scan1 (+) <~ source [1..5]
-- [1,3,6,10,15]
--
scan1 :: forall k a. (Category k) => (a -> a -> a) -> Translate (Is a) (k a)
scan1 func =
  let step t = t `seq` pure
             $ Yield t
             $ encased
             $ Await (step . func t)
                     id
                     (pure Stop)
  in  encased $ Await step id (pure Stop)
{-# INLINABLE scan1 #-}

-- |
-- Like 'scan' only uses supplied function to map and uses Monoid for
-- associative operation
--
-- Examples:
--
-- >>> run $ mapping getSum <~ scanMap Sum <~ source [1..5]
-- [0,1,3,6,10,15]
--
scanMap :: (Category k, Monoid b) => (a -> b) -> Translate (Is b) (k a)
scanMap f = scan (\b a -> mappend b (f a)) mempty
{-# INLINABLE scanMap #-}

-- |
-- Construct a 'Process' from a left-folding operation.
--
-- Like 'scan', but only yielding the final value.
--
-- It may be useful to consider this alternative signature
-- @
-- 'fold' :: (a -> b -> a) -> a -> Process b a
-- @
--
-- This can be constructed from a plan with
-- @
-- fold :: Category k => (a -> b -> a) -> a -> Machine (k b) a
-- fold func seed = construct $ go seed where
--   go cur = do
--     next <- await <|> yield cur *> stop
--     go $! func cur next
-- @
--
-- Examples:
--
-- >>> run $ fold (+) 0 <~ source [1..5]
-- [15]
--
-- >>> run $ fold (\a _ -> a + 1) 0 <~ source [1..5]
-- [5]
--
fold :: (Category k) => (a -> b -> a) -> a -> Translate (Is a) (k b)
fold func =
  let step t = t `seq` pure $ Await (step . func t)
                     id
                     (pure $ Yield t stopped)
  in machineT . step
{-# INLINABLE fold #-}

-- |
-- 'fold1' is a variant of 'fold' that has no starting value argument
--
-- This can be constructed from a plan with
-- @
-- fold1 :: Category k => (a -> a -> a) -> Machine (k a) a
-- fold1 func = construct $ await >>= go where
--   go cur = do
--     next <- await <|> yield cur *> stop
--     go $! func cur next
-- @
--
-- Examples:
--
-- >>> run $ fold1 (+) <~ source [1..5]
-- [15]
--
fold1 :: (Category k) => (a -> a -> a) -> Translate (Is a) (k a)
fold1 func =
  let step t = t `seq` pure
             $ Await (step . func t)
                     id
                     (pure $ Yield t stopped)
  in  encased $ Await step id (pure Stop)
{-# INLINABLE fold1 #-}

-- | Break each input into pieces that are fed downstream
-- individually.
--
-- This can be constructed from a plan with
-- @
-- asParts :: Foldable f => Process (f a) a
-- asParts = repeatedly $ await >>= traverse_ yield
-- @
--
-- Examples:
--
-- >>> run $ asParts <~ source [[1..3],[4..6]]
-- [1,2,3,4,5,6]
--
asParts :: (Category k, Foldable f, Applicative m) => TranslateT m (Is a) (k (f a))
asParts =
  let step = Await (pure . foldr (\b s -> Yield b (encased s)) step)
                   id
                   (pure Stop)
  in  encased step
{-# INLINABLE asParts #-}

-- | Break each input into pieces that are fed downstream
-- individually.
--
-- Alias for @asParts@
--
flattened :: (Category k, Foldable f, Applicative m) => TranslateT m (Is a) (k (f a))
flattened = asParts
{-# INLINABLE flattened #-}

-- | @sinkPart_ toParts sink@ creates a process that uses the
-- @toParts@ function to break input into a tuple of @(passAlong,
-- sinkPart)@ for which the second projection is given to the supplied
-- @sink@ 'ProcessT' (that produces no output) while the first
-- projection is passed down the pipeline.
sinkPart_ :: forall m a b c. Monad m => (a -> (b,c)) -> ProcessT m c (Is Void) -> ProcessT m a (Is b)
sinkPart_ p m = TranslateT $ \Refl -> go <$> runTranslateT m Refl
  where
    go :: Step m (Is Void) (Is c) Void -> Step m (Is b) (Is a) b
    go v = case v of
          Stop -> Stop
          Yield o _ -> absurd o
          Await f Refl ff ->
            Await (\x -> let (keep,sink) = p x
                         in Yield keep . encased . go <$> f sink)
                  Refl
                  (go <$> ff)

-- | Apply a monadic function to each element of a 'ProcessT'.
--
-- This can be constructed from a plan with
-- @
-- autoM :: Monad m => (a -> m b) -> ProcessT m a b
-- autoM :: (Category k, Monad m) => (a -> m b) -> MachineT m (k a) b
-- autoM f = repeatedly $ await >>= lift . f >>= yield
-- @
--
-- Examples:
--
-- >>> runT $ autoM Left <~ source [3, 4]
-- Left 3
--
-- >>> runT $ autoM Right <~ source [3, 4]
-- Right [3,4]
--
autoM :: (Monad m) => (a -> m b) -> ProcessT m a (Is b)
autoM f =
    loop
  where
    loop = encased $
        Await (\t -> flip Yield loop <$> f t) id (pure Stop)
{-# INLINABLE autoM #-}

-- |
-- Skip all but the final element of the input
--
-- This can be constructed from a plan with
-- @
-- 'final' :: 'Process' a a
-- final :: Category k => Machine (k a) a
-- final = construct $ await >>= go where
--   go prev = do
--     next <- await <|> yield prev *> stop
--     go next
-- @
--
-- Examples:
--
-- >>> runT $ final <~ source [1..10]
-- [10]
-- >>> runT $ final <~ source []
-- []
--
final :: Translate k k
final = TranslateT $ \req ->
  let step x = pure $ Await step req (emit x)
      emit x = pure (Yield x stopped)
  in pure $ Await (\t -> step t) req (pure Stop)
{-# INLINABLE final #-}

-- |
-- Skip all but the final element of the input.
-- If the input is empty, the default value is emitted
--
-- This can be constructed from a plan with
-- @
-- 'finalOr' :: a -> 'Process' a a
-- finalOr :: Category k => a -> Machine (k a) a
-- finalOr = construct . go where
--   go prev = do
--     next <- await <|> yield prev *> stop
--     go next
-- @
--
-- Examples:
--
-- >>> runT $ finalOr (-1) <~ source [1..10]
-- [10]
-- >>> runT $ finalOr (-1) <~ source []
-- [-1]
--
finalOr :: Category k => a -> Translate (Is a) (k a)
finalOr x = TranslateT $ \Refl ->
  let step x = pure $ Await step id (emit x)
      emit x = pure (Yield x stopped)
  in step x
{-# INLINABLE finalOr #-}

-- |
-- Intersperse an element between the elements of the input
--
-- @
-- 'intersperse' :: a -> 'Process' a a
-- @
intersperse :: Category k => a -> Machine (k a) a
intersperse sep = construct $ await >>= go where
  go cur = do
    yield cur
    next <- await
    yield sep
    go next

-- |
-- Return the maximum value from the input
largest :: (Category k, Ord a) => Translate (Is a) (k a)
largest = fold1 max
{-# INLINABLE largest #-}

-- |
-- Return the minimum value from the input
smallest :: (Category k, Ord a) => Translate (Is a) (k a)
smallest = fold1 min
{-# INLINABLE smallest #-}

-- |
-- Convert a stream of actions to a stream of values
--
-- This can be constructed from a plan with
-- @
-- sequencing :: Monad m => (a -> m b) -> ProcessT m a b
-- sequencing :: (Category k, Monad m) => MachineT m (k (m a)) a
-- sequencing = repeatedly $ do
--   ma <- await
--   a  <- lift ma
--   yield a
-- @
--
-- Examples:
--
-- >>> runT $ sequencing <~ source [Just 3, Nothing]
-- Nothing
--
-- >>> runT $ sequencing <~ source [Just 3, Just 4]
-- Just [3,4]
--
sequencing :: (Monad m) => ProcessT m (m b) (Is b)
sequencing = autoM id
{-# INLINABLE sequencing #-}

-- |
-- Apply a function to all values coming from the input
--
-- This can be constructed from a plan with
-- @
-- mapping :: Category k => (a -> b) -> Machine (k a) b
-- mapping f = repeatedly $ await >>= yield . f
-- @
--
-- Examples:
--
-- >>> runT $ mapping (*2) <~ source [1..3]
-- [2,4,6]
--
mapping :: (a -> b) -> Process a (Is b)
mapping f =
    loop
  where
    loop = encased (Await (\t -> pure (Yield (f t) loop)) id (pure Stop))
{-# INLINABLE mapping #-}

-- |
-- Apply an effectful to all values coming from the input.
--
-- Alias to 'autoM'.
traversing :: (Monad m) => (a -> m b) -> ProcessT m a (Is b)
traversing = autoM

-- |
-- Parse 'Read'able values, only emitting the value if the parse succceeds.
-- This 'Machine' stops at first parsing error
reading :: (Category k, Read a) => Machine (k String) a
reading = repeatedly $ do
  s <- await
  case reads s of
    [(a, "")] -> yield a
    _         -> stop

-- |
-- Convert 'Show'able values to 'String's
showing :: (Show a) => Process a (Is String)
showing = mapping show
{-# INLINABLE showing #-}

strippingPrefixWith ::
    forall m j k. (Monad m) =>
    (forall t . j t -> t -> t -> Bool) ->
    TranslateT m j k -> TranslateT m j k -> TranslateT m j k
strippingPrefixWith eq mp mb = TranslateT $ \req -> go req (runTranslateT mp req)
  where
    go :: j t -> MStep m j k t -> MStep m j k t
    go req mstep = mstep >>= \v -> case v of
      Stop          -> runTranslateT mb req
      Yield b k     -> verify req b k (runTranslateT mb req)
      Await f ki ff ->
        return $ Await (go req . f) ki (go req ff)
    verify :: j t -> t -> TranslateT m j k -> MStep m j k t -> MStep m j k t
    verify req b nxt cur = cur >>= \u -> case u of
      Stop -> return Stop
      Yield b' nxt'
        | eq req b b' -> runTranslateT (strippingPrefixWith eq nxt nxt') req
        | otherwise -> return Stop
      Await f ki ff ->
        return $ Await (verify req b nxt . f)
                    ki (verify req b nxt ff)

-- |
-- 'strippingPrefix' @mp mb@ Drops the given prefix from @mp@. It stops if @mb@
-- did not start with the prefix given, or continues streaming after the
-- prefix, if @mb@ did.
strippingPrefix :: (Eq b, Monad m)
                => TranslateT m (Is b) k
                -> TranslateT m (Is b) k
                -> TranslateT m (Is b) k
strippingPrefix = strippingPrefixWith $ \Refl -> (==)
