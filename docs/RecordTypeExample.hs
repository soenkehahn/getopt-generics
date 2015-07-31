{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import System.Console.GetOpt.Generics

-- All you have to do is to define a type and derive some instances:

data Options
  = Options {
    port :: Int,
    daemonize :: Bool,
    config :: Maybe FilePath
  }
  deriving (Show, GHC.Generics.Generic)

instance System.Console.GetOpt.Generics.Generic Options
instance HasDatatypeInfo Options

-- Then you can use `getArguments` to create a command-line argument parser:

main :: IO ()
main = do
  options <- getArguments
  print (options :: Options)
