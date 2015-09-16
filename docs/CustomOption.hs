{-# LANGUAGE DeriveDataTypeable #-}

module CustomOption where

import WithCli

data File = File FilePath
  deriving (Show, Typeable)

instance Option File where
  argumentType Proxy = "custom-file-type"
  parseArgument f = Just (File f)

instance HasOptions File where
  fromArguments = fromArgumentsOption

main :: IO ()
main = withCli $ \ file -> do
  print (file :: File)
