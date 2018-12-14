{-# OPTIONS_GHC -Wall -Wcompat #-}

module Main where

import Distribution.Extra.Doctest

main :: IO ()
main = defaultMainWithDoctests "doctest"
