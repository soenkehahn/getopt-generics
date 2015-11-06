{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Test02 where

import WithCli

data Options
  = Options {
    port :: Int,
    daemonize :: Bool,
    config :: Maybe FilePath,
    args :: [String]
  }
  deriving (Show, Generic, HasArguments)

main :: IO ()
main = withCli run

run :: Options -> IO ()
run = print
