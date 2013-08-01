{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Process
-- Copyright   :  (C) 2012-2013 Edward Kmett
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
  , Automaton(..)
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
  ) where

import Control.Applicative
import Control.Category
import Control.Monad (when, replicateM_)
import Data.Foldable
import Data.Machine.Type
import Prelude hiding ((.),id)

infixr 9 <~
infixl 9 ~>

-------------------------------------------------------------------------------
-- Processes
-------------------------------------------------------------------------------

-- | A @'Process' a b@ is a stream transducer that can consume values of type @a@
-- from its input, and produce values of type @b@ for its output.
type Process a = Machine ((->) a)

-- | An 'Automaton' is can be automatically lifted into a 'Process'
class Automaton k where
  auto :: k a b -> Process a b

instance Automaton (->) where
  auto f = repeatedly (fmap f await)

-- | The trivial 'Process' that simply repeats each input it receives.
echo :: Process a a
echo = repeatedly await

-- | A 'Process' that prepends the elements of a 'Foldable' onto its input, then repeats its input from there.
prepended :: Foldable f => f a -> Process a a
prepended fa = foldMap return fa <|> echo

-- | A 'Process' that only passes through inputs that match a predicate.
filtered :: (a -> Bool) -> Process a a
filtered p = repeatedly $ do
  i <- await
  guard (p i)

-- | A 'Process' that drops the first @n@, then repeats the rest.
dropping :: Int -> Process a a
dropping n = replicateM_ n (await *> empty) <|> echo

-- | A 'Process' that passes through the first @n@ elements from its input then stops
taking :: Int -> Process a a
taking n = replicateM_ n await

-- | A 'Process' that passes through elements until a predicate ceases to hold, then stops
takingWhile :: (a -> Bool) -> Process a a
takingWhile p = repeatedly $ await >>= \v -> if p v then return v else empty

-- | A 'Process' that drops elements while a predicate holds
droppingWhile :: (a -> Bool) -> Process a a
droppingWhile p = loop <|> echo where
  loop = await >>= \v -> if p v then loop else yield v

-- | Chunk up the input into `n` element lists.
--
-- Avoids returning empty lists and deals with the truncation of the last group.
buffered :: Int -> Process a [a]
buffered = repeatedly . go [] where
  go [] 0  = empty
  go acc 0 = pure (reverse acc)
  go acc n = do
    i <- await <|> pure (reverse acc) *> empty
    go (i:acc) $! n-1
n
-- | Build a new 'Machine' by adding a 'Process' to the output of an old 'Machine'
--
-- @
-- ('<~') :: 'Process' b c -> 'Process' a b -> 'Process' a c
-- ('<~') :: 'Process' c d -> 'Data.Machine.Tee.Tee' a b c -> 'Data.Machine.Tee.Tee' a b d
-- ('<~') :: 'Process' b c -> 'Machine' k b -> 'Machine' k c
-- @
(<~) :: Process a b -> Machine m a -> Machine m b
ml <~ mr = Machine $ \ky kh ka -> runMachine ml (\b k -> ky b (k <~ mr)) kh $ \mf kf ks ->
  runMachine mr (\a k -> ks (mf a) <~ k) (kf <~ empty) $ \mg kg kt -> ka mg (ml <~ kt) (\i -> ml <~ kg i)

{-
Stop         <~ _             = Stop
Yield o k    <~ ma            = Yield o (k <~ ma)
Await _ _ ff <~ Stop          = ff <~ empty
Await f m _  <~ Yield o k     = f (m o) <~ k
ab           <~ Await g kg fg = Await (\i -> ab <~ g i) kg (ab <~ fg)
-}

-- | Flipped ('<~').
(~>) :: Machine m b -> Process b c -> Machine m c
ma ~> mp = mp <~ ma

-- | Feed a 'Process' some input.
supply :: [a] -> Process a b -> Process a b
supply = undefined
-- supply xs0 m = Machine $ \ky kh ka -> runMachine m (\a r xs -> ky a (r xs)) ( ) ( ) xs0

{-
supply []     m             = m
supply _      Stop          = Stop
supply xxs    (Yield o k)   = Yield o (supply xxs k)
supply (x:xs) (Await f g _) = supply xs (f (g x))
-}
