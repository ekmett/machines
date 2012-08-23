-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Unread
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  rank 2 types
--
----------------------------------------------------------------------------
module Data.Machine.Unread
  ( Unread(..)
  , peek
  ) where

import Data.Machine.Plan

data Unread a r
  = Unread a (() -> r)
  | Read (a -> r)

-- | Peek at the next value in the input stream without consuming it
peek :: Plan Unread a b a
peek = do
  a <- awaits Read
  awaits (Unread a)
  return a

-- | Push back into the input stream
unread :: a -> Plan Unread a b ()
unread a = awaits (Unread a)

-- TODO: make this a class?
