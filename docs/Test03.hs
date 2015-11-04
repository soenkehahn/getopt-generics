{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Test03 where

import qualified GHC.Generics
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
  deriving (Show, GHC.Generics.Generic)

instance HasArguments A

data B
  = B {
    bb :: String
  }
  deriving (Show, GHC.Generics.Generic)

instance HasArguments B
