{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Test04 where

import qualified GHC.Generics
import           WithCli

main :: IO ()
main = withCli $ \ a b -> do
  print ((a, b) :: (A, B))

data A
  = A {
    aa :: String
  }
  deriving (Show, GHC.Generics.Generic)

instance Generic A
instance HasDatatypeInfo A
instance HasArguments A

data B
  = B {
    bb :: String
  }
  deriving (Show, GHC.Generics.Generic)

instance Generic B
instance HasDatatypeInfo B
instance HasArguments B
