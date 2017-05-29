{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.Monoid
import Test.QuickCheck
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import Data.Machine


-- Data.Machine.Tee
prop_teeT :: NonNegative Int -> Bool
prop_teeT (NonNegative n) = (getSum $ execWriter $ runT_ $ teeT f g stopped <~ repeated ()) == n
  where
    f = construct $ replicateM_ n (lift (tell (Sum 1)) >> awaits L)
    g = construct $ forever (await >> yield ())

main :: IO ()
main = $defaultMainGenerator
