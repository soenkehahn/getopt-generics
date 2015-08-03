
module Test01 where

import SimpleCLI

main :: IO ()
main = simpleCLI $ \ i b ->
  print (i :: Int, b :: Bool)
