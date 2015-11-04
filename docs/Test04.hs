{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Test04 where

import qualified GHC.Generics
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
  deriving (Show, GHC.Generics.Generic)

instance HasArguments A

data B
  = B {
    bb :: String
  }
  deriving (Show, GHC.Generics.Generic)

instance HasArguments B
