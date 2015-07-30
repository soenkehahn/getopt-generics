module SimpleExample where

import           System.Console.GetOpt.Generics

main :: IO ()
main = simpleCLI myMain

myMain :: String -> Int -> Bool -> IO ()
myMain s i b = print (s, i, b)
