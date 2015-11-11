{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module RecordType where

import WithCli

data Options
  = Options {
    port :: Int,
    daemonize :: Bool,
    config :: Maybe FilePath
  }
  deriving (Show, Generic, HasArguments)

main :: IO ()
main = withCli run

run :: Options -> IO ()
run = print
