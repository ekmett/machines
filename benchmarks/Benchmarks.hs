module Main (main) where

import Control.Monad (void)
import Control.Monad.Identity
import Criterion.Main
import qualified Data.Conduit      as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as C
import qualified Data.Machine      as M
import qualified Pipes             as P
import qualified Pipes.Prelude     as P

value :: Int
value = 1000000

drainM :: M.ProcessT Identity Int o -> ()
drainM m = runIdentity $ M.runT_ (sourceM M.~> m)

drainP :: P.Proxy () Int () a Identity () -> ()
drainP p = runIdentity $ P.runEffect $ P.for (sourceP P.>-> p) P.discard

drainC :: C.Conduit Int Identity a -> ()
drainC c = runIdentity $ (sourceC C.$= c) C.$$ C.sinkNull

drainSC :: C.Sink Int Identity b -> ()
drainSC c = runIdentity $ void $ sourceC C.$$ c

sourceM = M.enumerateFromTo 1 value
sourceC = C.enumFromTo 1 value
sourceP = P.each [1..value]

main :: IO ()
main =
  defaultMain
  [ bgroup "map"
      [ bench "machines" $ whnf drainM (M.auto (+1))
      , bench "pipes" $ whnf drainP (P.map (+1))
      , bench "conduit" $ whnf drainC (C.map (+1))
      ]
  , bgroup "drop"
      [ bench "machines" $ whnf drainM (M.dropping value)
      , bench "pipes" $ whnf drainP (P.drop value)
      , bench "conduit" $ whnf drainC (C.drop value)
      ]
  , bgroup "dropWhile"
      [ bench "machines" $ whnf drainM (M.droppingWhile (<= value))
      , bench "pipes" $ whnf drainP (P.dropWhile (<= value))
      , bench "conduit" $ whnf drainC (CC.dropWhile (<= value))
      ]
  , bgroup "scan"
      [ bench "machines" $ whnf drainM (M.scan (+) 0)
      , bench "pipes" $ whnf drainP (P.scan (+) 0 id)
      , bench "conduit" $ whnf drainC (CC.scanl (+) 0)
      ]
  , bgroup "take"
      [ bench "machines" $ whnf drainM (M.taking value)
      , bench "pipes" $ whnf drainP (P.take value)
      , bench "conduit" $ whnf drainSC (C.take value)
      ]
  , bgroup "takeWhile"
      [ bench "machines" $ whnf drainM (M.takingWhile (<= value))
      , bench "pipes" $ whnf drainP (P.takeWhile (<= value))
      , bench "conduit" $ whnf drainSC (CC.takeWhile (<= value) C.=$= C.sinkNull)
      ]
  , bgroup "fold"
      [ bench "machines" $ whnf drainM (M.fold (+) 0)
      , bench "pipes" $ whnf (P.fold (+) 0 id) sourceP
      , bench "conduit" $ whnf drainSC (C.fold (+) 0)
      ]
  ]
