module Main where

import System.Directory as D

main :: IO ()
main = do
  files <- D.listDirectory "."
  print files
