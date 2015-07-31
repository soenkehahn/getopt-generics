{-# LANGUAGE DeriveDataTypeable #-}

import           Data.Typeable
import           System.Console.GetOpt.Generics

data File = File FilePath
  deriving (Show, Typeable)

instance Option File where
  argumentType Proxy = "custom-file-type"
  parseArgument f = Just (File f)

main :: IO ()
main = simpleCLI $ \ file -> do
  print (file :: File)
