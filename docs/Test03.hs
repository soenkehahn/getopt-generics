{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Test03 where

import           WithCli

main :: IO ()
main = withCli run

run :: (A, B) -> IO ()
run options = do
  print (options :: (A, B))

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
