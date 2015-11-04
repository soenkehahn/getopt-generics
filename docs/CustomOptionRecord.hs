{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module CustomOptionRecord where

import WithCli

data File = File FilePath
  deriving (Show, Typeable)

instance Argument File where
  argumentType Proxy = "custom-file-type"
  parseArgument f = Just (File f)

data Options
  = Options {
    file :: File
  }
  deriving (Show, Generic, HasArguments)

instance HasArguments File where
  argumentsParser = atomicArgumentsParser

main :: IO ()
main = withCli run

run :: Options -> IO ()
run = print
