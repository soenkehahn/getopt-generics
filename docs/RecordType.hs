{-# LANGUAGE DeriveGeneric #-}

module RecordType where

import           System.Console.GetOpt.Generics

-- All you have to do is to define a type and derive an instance for Generic:

data Options
  = Options {
    port :: Int,
    daemonize :: Bool,
    config :: Maybe FilePath
  }
  deriving (Show, Generic)

-- Then you can use `getArguments` to create a command-line argument parser:

main :: IO ()
main = do
  options <- getArguments
  print (options :: Options)
