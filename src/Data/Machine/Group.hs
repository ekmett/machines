{-# LANGUAGE GADTs #-}
module Data.Machine.Group
  ( groupingOn
  , taggedBy
  , partitioning
  , starve
  , awaitUntil
  )where
import Data.Machine
import qualified Data.Machine.Group.General as Group

-- | Using a function to signal group changes, apply a machine independently over each group.
groupingOn :: Monad m => (a -> a -> Bool) -> ProcessT m a b -> ProcessT m a b
groupingOn = Group.groupingOn_

-- | Mark a transition point between two groups as a function of adjacent elements.
-- Examples
--
-- >>> runT $ supply [1,2,2] (taggedBy (==))
-- [Right 1,Left (),Right 2,Right 2]
taggedBy :: Monad m => (a -> a -> Bool) -> ProcessT m a (Either () a)
taggedBy = Group.taggedOn_


-- | Run a machine multiple times over partitions of the input stream specified by
-- Left () values.
partitioning :: Monad m => ProcessT m a b -> ProcessT m (Either () a) b
partitioning = Group.partitioning_

-- | Read inputs until a condition is met, then behave as cont with
-- | input matching condition as first input of cont.
-- | If await fails, stop.
awaitUntil :: Monad m => (a -> Bool) -> (a -> ProcessT m a b) -> ProcessT m a b
awaitUntil = Group.awaitUntil
