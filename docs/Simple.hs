module Simple where

import SimpleCLI

main :: IO ()
main = simpleCLI myMain

myMain :: String -> Int -> Bool -> IO ()
myMain s i b = print (s, i, b)
