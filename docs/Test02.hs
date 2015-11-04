{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Test02 where

import           WithCli

data Options
  = Options {
    port :: Int,
    daemonize :: Bool,
    config :: Maybe FilePath,
    args :: [String]
  }
  deriving (Show, Generic)

instance HasArguments Options

main :: IO ()
main = withCli run

run :: Options -> IO ()
run = print
