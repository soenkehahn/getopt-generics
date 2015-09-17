module Simple where

import WithCli

main :: IO ()
main = withCli run

run :: String -> Int -> Bool -> IO ()
run s i b = print (s, i, b)
