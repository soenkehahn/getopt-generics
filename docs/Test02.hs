{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Test02 where

import qualified GHC.Generics
import           WithCli

-- All you have to do is to define a type and derive some instances:

data Options
  = Options {
    port :: Int,
    daemonize :: Bool,
    config :: Maybe FilePath
  }
  deriving (Show, GHC.Generics.Generic)

instance Generic Options
instance HasDatatypeInfo Options
instance HasArguments Options

-- Then you can use `getArguments` to create a command-line argument parser:

main :: IO ()
main = withCli $ \ options -> do
  print (options :: Options)
