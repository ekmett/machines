{-# LANGUAGE Rank2Types, GADTs, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Process
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  rank-2, GADTs
--
----------------------------------------------------------------------------
module Data.Machine.Process
  (
  -- * Processes
    Process
  , ProcessT
  , Automaton(..)
  -- ** Common Processes
--  , after
  , supply
--  , pipe
  , prepended
  , filtered
  , dropping
  , taking
  , droppingWhile
  , takingWhile
  , buffered
  ) where

import Control.Applicative
import Control.Category
import Control.Monad (when, replicateM_)
import Data.Foldable
import Data.Machine.Plan
import Data.Machine.Type
import Prelude hiding ((.),id)

-------------------------------------------------------------------------------
-- Processes
-------------------------------------------------------------------------------

-- | A @'Process' a b@ is a stream transducer that can consume values of type @a@
-- from its input, and produce values of type @b@ for its output.
type Process a b = Machine (->) a b

-- | A @'ProcessT' m a b@ is a stream transducer that can consume values of type @a@
-- from its input, and produce values of type @b@ for its output, with side-effects
-- in the 'Monad' @m@.
type ProcessT m a b = MachineT m (->) a b

-- | An 'Automaton' is can be automatically lifted into a 'Process'
class Automaton k where
  auto :: k a b -> Process a b

instance Automaton (->) where
  auto f = repeatedly $ do
    i <- await
    yield (f i)

-- | A process that prepends the elements of a 'Foldable' onto its input, then repeats its input from there.
prepended :: Foldable f => f a -> Process a a
prepended = before id . traverse_ yield

-- | A process that only passes through inputs that match a predicate.
filtered :: (a -> Bool) -> Process a a
filtered p = repeatedly $ do
  i <- await
  when (p i) $ yield i

-- | A process that drops the first @n@, then repeats the rest.
dropping :: Int -> Process a a
dropping n = before id $ replicateM_ n await

-- | A process that passes through the first @n@ elements from its input then stops
taking :: Int -> Process a a
taking n = construct . replicateM_ n $ await >>= yield

-- | A process that passes through elements until a predicate ceases to hold, then stops
takingWhile :: (a -> Bool) -> Process a a
takingWhile p = repeatedly $ await >>= \v -> if p v then yield v else stop

-- | A process that drops elements while a predicate holds
droppingWhile :: (a -> Bool) -> Process a a
droppingWhile p = before id loop where
  loop = await >>= \v -> if p v then loop else yield v

-- | Chunk up the input into `n` element lists.
-- The last list may be shorter.
buffered :: Int -> Process a [a]
buffered = repeatedly . go [] where
  go [] 0  = stop
  go acc 0 = yield (reverse acc)
  go acc n = do
    i <- await <|> yield (reverse acc) *> stop
    go (i:acc) $! n-1

{-
-- | Bolt a 'Process' on the end of any 'Machine'.
pipe :: Process b c -> Machine k a b -> Machine k a c
pipe Stop            _                = Stop
pipe (Yield a as)    sf               = Yield a (pipe as sf)
pipe (Await f kir _) (Yield b bs)     = pipe (fmap f kir b) bs
pipe (Await _ _ g)   Stop             = pipe g Stop
pipe sf              (Await g kir fg) = Await (fmap (pipe sf) g) kir (pipe sf fg)
-}

{-
after :: Machine k a b -> Process b c -> Machine k a c
after _ Stop                            = Stop
after sf (Yield a as)    = Yield a (after sf as)
after (Yield b bs) (Await f kir _)  = after bs (fmap f kir b)
after Stop (Await _ _ g)   = after Stop g
after (Await g kir fg) sf = Await (fmap (`after` sf) g) kir (after fg sf)
-}

-- | Feed a 'Process' some input.
supply :: Monad m => [a] -> ProcessT m a b -> ProcessT m a b
supply []         m = m
supply xxs@(x:xs) m = MachineT $ runMachineT m >>= \v -> case v of
  Stop -> return Stop
  Await f kir _ -> runMachineT $ supply xs (fmap f kir x)
  Yield o k -> return $ Yield o (supply xxs k)
