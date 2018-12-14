module Main where

import           Build_doctests (flags, module_sources, pkgs)
import           Test.DocTest

customFlags :: [String]
customFlags =
  [ "-Wno-deprecations"
  ]

main :: IO ()
main = do
  let args = flags ++ customFlags ++ pkgs ++ module_sources
  doctest args
