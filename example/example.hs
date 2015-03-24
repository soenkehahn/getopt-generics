{-# LANGUAGE DeriveGeneric #-}

import           Generics.SOP
import qualified GHC.Generics
import           System.Console.Args.Generics
import           System.Environment

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
  withArguments $ \ myFoo -> do
    print $ bar myFoo
    print $ baz myFoo
    print $ qux myFoo
