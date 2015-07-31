{-# LANGUAGE DeriveGeneric #-}

import qualified GHC.Generics
import           System.Console.GetOpt.Generics

-- we specify a data type that maps to the commandline arguments

data Foo
  = Foo {
      bar :: String
    , baz :: Maybe String
    , qux :: Int
    }
    deriving (Eq, Show, GHC.Generics.Generic)

instance Generic Foo
instance HasDatatypeInfo Foo

-- give it go

main :: IO ()
main = do
  myFoo <- getArguments
  print $ bar myFoo
  print $ baz myFoo
  print $ qux myFoo
