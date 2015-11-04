{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Test04 where

import WithCli

main :: IO ()
main = withCli run

run :: A -> B -> IO ()
run a b = do
  print (a, b)

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
