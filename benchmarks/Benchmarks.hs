module Main (main) where

import Control.Monad.Identity
import Criterion.Main
import qualified Data.Conduit      as C
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

sourceM = M.enumerateFromTo 1 value
sourceC = C.enumFromTo 1 value
sourceP = P.each [1..value]

main :: IO ()
main =
  defaultMain
  [ bgroup "mapping"
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
      ]
  ]
