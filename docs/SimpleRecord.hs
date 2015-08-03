{-# LANGUAGE DeriveGeneric #-}

module SimpleRecord where

import qualified GHC.Generics
import           System.Console.GetOpt.Generics

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
instance HasOptions Options

-- Then you can use `simpleCLI` to create a command-line argument parser:

main :: IO ()
main = simpleCLI $ \ options -> do
  print (options :: Options)
