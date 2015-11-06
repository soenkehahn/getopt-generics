{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Test03 where

import WithCli

main :: IO ()
main = withCli run

run :: (A, B) -> IO ()
run options = do
  print (options :: (A, B))

data A
  = A {
    aa :: String
  }
  deriving (Show, Generic, HasArguments)

data B
  = B {
    bb :: String
  }
  deriving (Show, Generic, HasArguments)
