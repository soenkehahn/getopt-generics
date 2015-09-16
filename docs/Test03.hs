{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Test03 where

import qualified GHC.Generics
import           WithCli

main :: IO ()
main = withCli $ \ options -> do
  print (options :: (A, B))

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

instance HasArguments (A, B)
