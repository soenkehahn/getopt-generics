-- fixme: into haddock
-- fixme: add explanation
module CustomOption where

import SimpleCLI

data File = File FilePath
  deriving (Show, Typeable)

instance Option File where
  argumentType Proxy = "custom-file-type"
  parseArgument f = Just (File f)

instance HasOptions File where
  fromArguments = fromArgumentsOption

main :: IO ()
main = simpleCLI $ \ file -> do
  print (file :: File)
