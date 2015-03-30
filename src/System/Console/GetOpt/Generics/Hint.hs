
module System.Console.GetOpt.Generics.Hint where

import           Data.Char
import           Data.Maybe

data Hint
  = Short String Char

slugify :: String -> String
slugify [] = []
slugify (x : xs)
  | isUpper x = '-' : toLower x : slugify xs
  | otherwise = x : slugify xs

mkShortOptions :: [Hint] -> String -> [Char]
mkShortOptions hints option =
    catMaybes $ map inner hints
  where
    inner :: Hint -> Maybe Char
    inner (Short hintOption short)
      | hintOption == option || hintOption == slugify option
        = Just short
      | otherwise
        = Nothing
