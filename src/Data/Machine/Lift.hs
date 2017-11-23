{-# LANGUAGE ScopedTypeVariables #-}

-- | Utilities for working with machines that run in transformed monads,
-- inspired by @Pipes.Lift@.
module Data.Machine.Lift (execStateM, catchExcept, runReaderM) where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.Machine.Type

-- | Given an initial state and a 'MachineT' that runs in @'StateT' s m@,
-- produce a 'MachineT' that runs in @m@.
execStateM :: Monad m => s -> TranslateT (StateT s m) j k -> TranslateT m j k
execStateM s m = TranslateT $ \req -> do
  let go ss mstp = do
        (stp, s') <- runStateT (runTranslateT m req) ss
        case stp of
          Stop -> return Stop
          Yield o m' -> return $ Yield o (execStateM s' m')
          Await f k q -> return $ Await (\i -> go s' $ f i) k (go s' q)
  go s (runTranslateT m req)
    
-- | 'catchExcept' allows a broken machine to be replaced without stopping the
-- assembly line.
catchExcept :: 
    forall m e j k. (Monad m) =>
    TranslateT (ExceptT e m) j k
    -> (e -> TranslateT (ExceptT e m) j k)
    -> TranslateT (ExceptT e m) j k
catchExcept m c = TranslateT $ \req -> do
  let go mstp = do
        step <- mstp `catchE` \e -> runTranslateT (catchExcept (c e) c) req
        case step of
          Stop -> return Stop
          Yield o m' -> return $ Yield o (catchExcept m' c)
          Await f k m' -> return $ Await (go . f) k (go m')
  go (runTranslateT m req)

-- | Given an environment and a 'MachineT' that runs in @'ReaderT' e m@,
-- produce a 'MachineT' that runs in @m@.
runReaderM :: Monad m => e -> TranslateT (ReaderT e m) k o -> TranslateT m k o
runReaderM e = fitM (flip runReaderT e)
