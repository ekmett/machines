{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
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
  , fold
  , scan
  , asParts
  , sinkPart_
  , autoM
  , final
  , finalOr
  , intersperse
  ) where

import Control.Applicative
import Control.Category
import Control.Monad (liftM, when, replicateM_)
import Control.Monad.Trans.Class
import Data.Foldable hiding (fold)
import Data.Machine.Is
import Data.Machine.Plan
import Data.Machine.Type
import Data.Void
import Prelude hiding ((.), id, mapM_)

infixr 9 <~
infixl 9 ~>

-------------------------------------------------------------------------------
-- Processes
-------------------------------------------------------------------------------

-- | A @'Process' a b@ is a stream transducer that can consume values of type @a@
-- from its input, and produce values of type @b@ for its output.
type Process a b = Machine (Is a) b

-- | A @'ProcessT' m a b@ is a stream transducer that can consume values of type @a@
-- from its input, and produce values of type @b@ and has side-effects in the
-- 'Monad' @m@.
type ProcessT m a b = MachineT m (Is a) b

-- | An 'Automaton' is can be automatically lifted into a 'Process'
class Automaton k where
  auto :: k a b -> Process a b

instance Automaton (->) where
  auto f = repeatedly $ do
    i <- await
    yield (f i)

instance Automaton Is where
  auto Refl = echo

-- | The trivial 'Process' that simply repeats each input it receives.
echo :: Process a a
echo = repeatedly $ do
  i <- await
  yield i

-- | A 'Process' that prepends the elements of a 'Foldable' onto its input, then repeats its input from there.
prepended :: Foldable f => f a -> Process a a
prepended = before echo . traverse_ yield

-- | A 'Process' that only passes through inputs that match a predicate.
filtered :: (a -> Bool) -> Process a a
filtered p = repeatedly $ do
  i <- await
  when (p i) $ yield i

-- | A 'Process' that drops the first @n@, then repeats the rest.
dropping :: Int -> Process a a
dropping n = before echo $ replicateM_ n await

-- | A 'Process' that passes through the first @n@ elements from its input then stops
taking :: Int -> Process a a
taking n = construct . replicateM_ n $ await >>= yield

-- | A 'Process' that passes through elements until a predicate ceases to hold, then stops
takingWhile :: (a -> Bool) -> Process a a
takingWhile p = repeatedly $ await >>= \v -> if p v then yield v else stop

-- | A 'Process' that drops elements while a predicate holds
droppingWhile :: (a -> Bool) -> Process a a
droppingWhile p = before echo loop where
  loop = await >>= \v -> if p v then loop else yield v

-- | Chunk up the input into `n` element lists.
--
-- Avoids returning empty lists and deals with the truncation of the final group.
buffered :: Int -> Process a [a]
buffered = repeatedly . go [] where
  go [] 0  = stop
  go acc 0 = yield (reverse acc)
  go acc n = do
    i <- await <|> yield (reverse acc) *> stop
    go (i:acc) $! n-1


-- | Build a new 'Machine' by adding a 'Process' to the output of an old 'Machine'
--
-- @
-- ('<~') :: 'Process' b c -> 'Process' a b -> 'Process' a c
-- ('<~') :: 'Process' c d -> 'Data.Machine.Tee.Tee' a b c -> 'Data.Machine.Tee.Tee' a b d
-- ('<~') :: 'Process' b c -> 'Machine' k b -> 'Machine' k c
-- @
(<~) :: Monad m => ProcessT m b c -> MachineT m k b -> MachineT m k c
mp <~ ma = MachineT $ runMachineT mp >>= \v -> case v of
  Stop          -> return Stop
  Yield o k     -> return $ Yield o (k <~ ma)
  Await f Refl ff -> runMachineT ma >>= \u -> case u of
    Stop          -> runMachineT $ ff <~ stopped
    Yield o k     -> runMachineT $ f o <~ k
    Await g kg fg -> return $ Await (\a -> MachineT (return v) <~ g a) kg (MachineT (return v) <~ fg)

-- | Flipped ('<~').
(~>) :: Monad m => MachineT m k b -> ProcessT m b c -> MachineT m k c
ma ~> mp = mp <~ ma

-- | Feed a 'Process' some input.
supply :: Monad m => [a] -> ProcessT m a b -> ProcessT m a b
supply []         m = m
supply xxs@(x:xs) m = MachineT $ runMachineT m >>= \v -> case v of
  Stop -> return Stop
  Await f Refl _ -> runMachineT $ supply xs (f x)
  Yield o k -> return $ Yield o (supply xxs k)

-- |
-- Convert a machine into a process, with a little bit of help.
--
-- @
-- 'process' 'Data.Machine.Tee.L' :: 'Data.Machine.Process.Process' a c -> 'Data.Machine.Tee.Tee' a b c
-- 'process' 'Data.Machine.Tee.R' :: 'Data.Machine.Process.Process' b c -> 'Data.Machine.Tee.Tee' a b c
-- 'process' 'id' :: 'Data.Machine.Process.Process' a b -> 'Data.Machine.Process.Process' a b
-- @
process :: Monad m => (forall a. k a -> i -> a) -> MachineT m k o -> ProcessT m i o
process f (MachineT m) = MachineT (liftM f' m) where
  f' (Yield o k)     = Yield o (process f k)
  f' Stop            = Stop
  f' (Await g kir h) = Await (process f . g . f kir) Refl (process f h)

-- |
-- Construct a 'Process' from a left-scanning operation.
--
-- Like 'fold', but yielding intermediate values.
--
-- @
-- 'scan' :: (a -> b -> a) -> a -> Process b a
-- @
scan :: Category k => (a -> b -> a) -> a -> Machine (k b) a
scan func seed = construct $ go seed where
  go cur = do
    yield cur
    next <- await
    go $ func cur next

-- |
-- Construct a 'Process' from a left-folding operation.
--
-- Like 'scan', but only yielding the final value.
--
-- @
-- 'fold' :: (a -> b -> a) -> a -> Process b a
-- @
fold :: Category k => (a -> b -> a) -> a -> Machine (k b) a
fold func seed = construct $ go seed where
  go cur = do
    next <- await <|> yield cur *> stop
    go (func cur next)

-- | Break each input into pieces that are fed downstream
-- individually.
asParts :: Foldable f => Process (f a) a
asParts = repeatedly $ await >>= mapM_ yield

-- | @sinkPart_ toParts sink@ creates a process that uses the
-- @toParts@ function to break input into a tuple of @(passAlong,
-- sinkPart)@ for which the second projection is given to the supplied
-- @sink@ 'ProcessT' (that produces no output) while the first
-- projection is passed down the pipeline.
sinkPart_ :: Monad m => (a -> (b,c)) -> ProcessT m c Void -> ProcessT m a b
sinkPart_ p = go
  where go m = MachineT $ runMachineT m >>= \v -> case v of
          Stop -> return Stop
          Yield _ k -> runMachineT $ go k
          Await f Refl ff -> return $
            Await (\x -> let (keep,sink) = p x
                         in encased . Yield keep $ go (f sink))
                  Refl
                  (go ff)

-- | Apply a monadic function to each element of a 'ProcessT'.
autoM :: Monad m => (a -> m b) -> ProcessT m a b
autoM f = repeatedly $ await >>= lift . f >>= yield

-- |
-- Skip all but the final element of the input
--
-- @
-- 'final' :: 'Process' a a
-- @
final :: Category k => Machine (k a) a
final = construct $ await >>= go where
  go prev = do
    next <- await <|> yield prev *> stop
    go next

-- |
-- Skip all but the final element of the input.
-- If the input is empty, the default value is emitted
--
-- @
-- 'finalOr' :: a -> 'Process' a a
-- @
finalOr :: Category k => a -> Machine (k a) a
finalOr = construct . go where
  go prev = do
    next <- await <|> yield prev *> stop
    go next

-- |
-- Intersperse an element between the elements of the input
--
-- @
-- 'intersperse' :: a -> 'Process' a a
-- @
intersperse :: Category k => a -> Machine (k a) a
intersperse sep = construct $ await >>= go where
  go cur = do
    next <- await <|> yield cur *> stop
    yield cur
    yield sep
    go next
