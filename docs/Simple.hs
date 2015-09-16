module Simple where

import WithCli

main :: IO ()
main = withCli myMain

myMain :: String -> Int -> Bool -> IO ()
myMain s i b = print (s, i, b)
