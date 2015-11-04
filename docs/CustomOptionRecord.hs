{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module CustomOptionRecord where

import           WithCli

data File = File FilePath
  deriving (Show, Typeable)

instance Argument File where
  argumentType Proxy = "custom-file-type"
  parseArgument f = Just (File f)

data Options
  = Options {
    file :: File
  }
  deriving (Show, Generic)

instance HasArguments Options
instance HasArguments File where
  argumentsParser = atomicArgumentsParser

main :: IO ()
main = withCli run

run :: Options -> IO ()
run = print
