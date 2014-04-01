module Main (main) where

import Control.Monad.Identity
import Criterion.Main
import qualified Data.Conduit      as C
import qualified Data.Conduit.List as C
import qualified Data.Machine      as M
import qualified Pipes             as P
import qualified Pipes.Prelude     as P

value :: Integer
value = 1000000

runM_ :: M.MachineT Identity k o -> ()
runM_ m = runIdentity $ M.runT_ m

runP :: P.Effect Identity a -> a
runP e = runIdentity $ P.runEffect e

runC :: C.Source Identity a -> ()
runC e = runIdentity $ e C.$$ C.sinkNull

main :: IO ()
main =
  defaultMain
  [ bgroup "mapping"
      [ bench "machines" $ nf
        (\s -> runM_ (s M.~> M.auto (+1)))
        (M.enumerateFromTo 1 value)

      , bench "pipes" $ nf
        (\n -> runP $ P.for (n P.>-> P.map (+1)) P.discard)
        (P.each [1..value])

      , bench "conduit" $ nf
        (\n -> runC (n C.$= C.map (+1)))
        (C.enumFromTo 1 value)
      ]
  ]
