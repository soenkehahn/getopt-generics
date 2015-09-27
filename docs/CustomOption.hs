{-# LANGUAGE DeriveDataTypeable #-}

module CustomOption where

import WithCli

data File = File FilePath
  deriving (Show, Typeable)

instance Argument File where
  argumentType Proxy = "custom-file-type"
  parseArgument f = Just (File f)

instance HasArguments File where
  argumentsParser = atomicArgumentsParser

main :: IO ()
main = withCli run

run :: File -> IO ()
run = print
