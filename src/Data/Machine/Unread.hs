{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Unread
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  GADTs
--
----------------------------------------------------------------------------
module Data.Machine.Unread
  ( Unread(..)
  , peek
  , unread
  ) where

import Data.Machine.Plan

-- | This is a simple process type that knows how to push back input.
data Unread a r where
  Unread :: a -> Unread a ()
  Read   :: Unread a a

-- | Peek at the next value in the input stream without consuming it
peek :: Plan (Unread a) b a
peek = do
  a <- awaits Read
  awaits (Unread a)
  return a

-- | Push back into the input stream
unread :: a -> Plan (Unread a) b ()
unread a = awaits (Unread a)

-- TODO: make this a class?
