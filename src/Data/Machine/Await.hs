{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x,y,z) 0
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Machine.Await
-- Copyright   :  (C) 2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  MPTCs
--
----------------------------------------------------------------------------
module Data.Machine.Await
  (
  -- * Awaiting
    Await(..)
  ) where

class Await i m | m -> i where
  -- | Wait for input.
  --
  -- @'await' = 'lift' 'id'@
  await :: m i

instance Await i ((->)i) where
  await = id
