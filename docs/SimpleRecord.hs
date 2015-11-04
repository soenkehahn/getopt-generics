{-# LANGUAGE DeriveGeneric #-}

module SimpleRecord where

import           System.Console.GetOpt.Generics

data Options
  = Options {
    port :: Int,
    daemonize :: Bool,
    config :: Maybe FilePath
  }
  deriving (Show, Generic)

instance HasArguments Options

main :: IO ()
main = withCli run

run :: Options -> IO ()
run = print
