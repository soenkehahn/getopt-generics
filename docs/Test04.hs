{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Test04 where

import           WithCli

main :: IO ()
main = withCli run

run :: A -> B -> IO ()
run a b = do
  print (a, b)

data A
  = A {
    aa :: String
  }
  deriving (Show, Generic)

instance HasArguments A

data B
  = B {
    bb :: String
  }
  deriving (Show, Generic)

instance HasArguments B
