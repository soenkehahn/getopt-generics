{-# LANGUAGE DeriveDataTypeable #-}

module CustomOption where

import WithCli

data File = File FilePath
  deriving (Show, Typeable)

instance Argument File where
  argumentType Proxy = "custom-file-type"
  parseArgument f = Just (File f)

instance HasArguments File where
  argumentsParser = atomicArgumentParser

main :: IO ()
main = withCli $ \ file -> do
  print (file :: File)
