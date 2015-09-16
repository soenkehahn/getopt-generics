
module Test01 where

import WithCli

main :: IO ()
main = withCli $ \ i b ->
  print (i :: Int, b :: Bool)
