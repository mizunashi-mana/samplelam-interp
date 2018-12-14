module Main where

import           Control.Monad
import           Language.Haskell.HLint
import qualified System.Environment     as Env
import           System.Exit            (exitFailure)

main :: IO ()
main = do
  args <- Env.getArgs
  hints <- hlint $ ["Setup.hs", "app", "src", "test"] ++ args
  unless (null hints) exitFailure
