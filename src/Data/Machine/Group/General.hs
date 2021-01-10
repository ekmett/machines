{-# LANGUAGE CPP   #-}
{-# LANGUAGE GADTs #-}

-- | Split up input streams into groups with separator values and process the
-- groups with their own 'MachineT'.

module Data.Machine.Group.General
  ( groupingOn
  , groupingOn_
  , groupingN
    -- * Tagging a stream
  , taggedState
  , taggedM
  , taggedOn
  , taggedOnM
  , taggedOn_
  , taggedAt
  , taggedAt_
  , taggedCount
    -- * Reset a machine for each group
  , partitioning
  , partitioning_
    -- * Helpers
  , starve
  , awaitUntil
  ) where

import           Control.Monad (guard)
import           Data.Machine

-- $setup
-- >>> import Control.Monad.Trans.Reader (ask, runReader)
-- >>> import Control.Monad (guard)
-- >>> import Control.Applicative ((<$))
-- >>> import Data.Machine

-- A strict tuple type.
data Strict2 a b = Strict2 !a !b

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

-- | Using a function to signal group changes, run a machine independently over
-- each group.
groupingOn_ :: Monad m => (a -> a -> Bool) -> ProcessT m a b -> ProcessT m a b
groupingOn_ f m = taggedOn_ f ~> partitioning_ m
{-# INLINE groupingOn_ #-}

-- | Using a function to signal group changes, run a machine independently over
-- each group with the value returned provided.
groupingOn :: Monad m => i -> (a -> a -> Maybe i) -> (i -> ProcessT m a b) -> ProcessT m a b
groupingOn i0 f m = taggedOn f ~> partitioning i0 m
{-# INLINE groupingOn #-}

-- | Run a machine repeatedly over 'n'-element segments of the stream, providing
-- an incrementing value to each run.
groupingN :: Monad m => Int -> (Int -> ProcessT m a b) -> ProcessT m a b
groupingN n m = taggedAt n 1 succ ~> partitioning 0 m
{-# INLINE groupingN #-}

-- | Mark a transition point between two groups when a state passing function
-- returns a 'Just' i.
-- Examples
--
-- >>> runT $ supply [1,3,3,2] (taggedState (-1) (\x y -> (even x <$ guard (x /= y), x)))
-- [Left False,Right 1,Left False,Right 3,Right 3,Left True,Right 2]
taggedState :: Monad m => s -> (a -> s -> (Maybe i, s)) -> ProcessT m a (Either i a)
taggedState s0 f = go s0
  where
    go s = encased
      $ Await (\x -> MachineT $ case f x s of
                  (Nothing, s') -> return $
                    Yield (Right x) (go s')
                  (Just i, s')  -> return $
                    Yield (Left i) (encased (Yield (Right x) (s' `seq` go s'))))
          Refl
          stopped
{-# INLINE taggedState #-}

-- | Mark a transition point between two groups when an action returns a 'Just'
-- i.  Could be useful for breaking up a stream based on time passed.
-- Examples
--
-- >>> let f x = do{ y <- ask; return (even x <$ guard (x > y)) }
-- >>> flip runReader 1 . runT $ supply [1,3,3,2] (taggedM f)
-- [Right 1,Left False,Right 3,Left False,Right 3,Left True,Right 2]
taggedM :: Monad m => (a -> m (Maybe i)) -> ProcessT m a (Either i a)
taggedM f = go
  where
    go = encased
      $ Await (\x -> MachineT $ f x >>= \v -> case v of
                  Nothing -> return $
                    Yield (Right x) go
                  Just i  -> return $
                    Yield (Left i) (encased (Yield (Right x) go))
              )
          Refl
          stopped
{-# INLINE taggedM #-}

-- | Mark a transition point between two groups as a function of adjacent
-- elements, and insert the value returned as the separator.
-- Examples
--
-- >>> runT $ supply [1,3,3,2] (taggedOn (\x y -> (x < y) <$ guard (x /= y)))
-- [Right 1,Left True,Right 3,Right 3,Left False,Right 2]
taggedOn :: Monad m => (a -> a -> Maybe i) -> ProcessT m a (Either i a)
taggedOn f = encased
  $ Await (\x0 -> encased $ Yield (Right x0) (taggedState x0 (\y x -> (f x y, y))))
      Refl
      stopped
{-# INLINE taggedOn #-}

-- | Mark a transition point between two groups using an action on adjacent
-- elements, and insert the value returned as the separator.
-- Examples
--
-- >>> let f x y = do{ z <- ask; return ((x + y <$ guard (z < x + y))) }
-- >>> flip runReader 5 . runT $ supply [1..5] (taggedOnM f)
-- [Right 1,Right 2,Right 3,Left 7,Right 4,Left 9,Right 5]
taggedOnM :: Monad m => (a -> a -> m (Maybe i)) -> ProcessT m a (Either i a)
taggedOnM f = encased $ Await go Refl stopped
  where
    go x = encased
      $ Yield (Right x) $ encased
          $ Await (\y -> MachineT $ f x y >>= \v -> case v of
                      Nothing -> runMachineT (go y)
                      Just z  -> return $ Yield (Left z) (go y))
              Refl
              stopped
{-# INLINE taggedOnM #-}

-- | Mark a transition point between two groups as a function of adjacent
-- elements.
-- Examples
--
-- >>> runT $ supply [1,2,2] (taggedOn_ (==))
-- [Right 1,Left (),Right 2,Right 2]
taggedOn_ :: Monad m => (a -> a -> Bool) -> ProcessT m a (Either () a)
taggedOn_ f = taggedOn (\x y -> guard (not (f x y)))
{-# INLINE taggedOn_ #-}

-- | Mark a transition point between two groups at every 'n' values, stepping
-- the separator by a function.
-- Examples
--
-- >>> runT $ supply [1..5] (taggedAt 2 True not)
-- [Right 1,Right 2,Left True,Right 3,Right 4,Left False,Right 5]
taggedAt :: Monad m => Int -> s -> (s -> s) -> ProcessT m a (Either s a)
taggedAt n s0 f = taggedState (Strict2 n s0) g
  where
    g _ (Strict2 i s) =
      if i <= 0 then (Just s, Strict2 (n-1) (f s))
        else (Nothing, Strict2 (i-1) s)
{-# INLINE taggedAt #-}

-- | Mark a transition point between two groups at every 'n' values.
-- Examples
--
-- >>> runT $ supply [1..5] (taggedAt_ 2)
-- [Right 1,Right 2,Left (),Right 3,Right 4,Left (),Right 5]
taggedAt_ :: Monad m => Int -> ProcessT m a (Either () a)
taggedAt_ n = taggedAt n () id
{-# INLINE taggedAt_ #-}

-- | Mark a transition point between two groups at every 'n' values, using the
-- counter as the separator.
-- Examples
--
-- >>> runT $ supply [1..5] (taggedCount 2)
-- [Right 1,Right 2,Left 1,Right 3,Right 4,Left 2,Right 5]
taggedCount :: Monad m => Int -> ProcessT m a (Either Int a)
taggedCount n = taggedAt n 1 succ
{-# INLINE taggedCount #-}

-- | Run a machine multiple times over partitions of the input stream specified
-- by 'Left' () values.
-- Examples
--
-- >>> let input = [Right 1,Left (),Right 3,Right 4,Left ()]
-- >>> runT $ supply input (partitioning_ (fold (flip (:)) []))
-- [[1],[4,3],[]]
partitioning_ :: Monad m => ProcessT m a b -> ProcessT m (Either () a) b
partitioning_ m = partitioning () (const m)
{-# INLINE partitioning_ #-}

-- | Run a machine multiple times over partitions of the input stream specified
-- by 'Left' i values, passing the 'i's to each 'MachineT' run.
-- Examples
--
-- >>> let input = [Right 1, Right 2,Left 1, Right 3,Left 2, Right 4]
-- >>> runT $ supply input (partitioning 0 (\x -> mapping (\y -> (x,y))))
-- [(0,1),(0,2),(1,3),(2,4)]
partitioning :: Monad m => i -> (i -> ProcessT m a b) -> ProcessT m (Either i a) b
partitioning i0 k0 = go (k0 i0) where
  go m = MachineT $ runMachineT m >>= \v -> case v of
    -- Machine stops (possibly before inputs)
    Stop -> runMachineT $ awaitUntil isLeft (const $ go (k0 i0))

    -- Machine yields a value
    Yield o r -> return $ Yield o (go r)

    -- Machine waits for a value
    Await f Refl r -> return $ Await g Refl (starve r $ encased Stop)
      where
        -- No change: unwrap input and give to underlying machine.
        g (Right a) = go (f a)
        -- New group: starve r, then wait for more input, restarting machine
        -- with next input.
        g (Left i)  = starve r $ go (k0 i)

-- | Read inputs until a condition is met, then behave as cont with input
-- matching condition as first input of cont.  If await fails, stop.
awaitUntil :: Monad m => (a -> Bool) -> (a -> ProcessT m a b) -> ProcessT m a b
awaitUntil f cont = encased $ Await g Refl stopped
  where g a = if f a then cont a else awaitUntil f cont
