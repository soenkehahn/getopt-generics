
module System.Console.GetOpt.Generics.FieldString (
  FieldString,
  mkFieldString,
  normalized,
  matches,
  renameUnnormalized,
  normalize,
 ) where

import           Data.Char
import           Data.Maybe

type FieldString = String

normalized :: FieldString -> String
normalized s = normalize s

mkFieldString :: String -> FieldString
mkFieldString = id
-- fixme: remove

matches :: String -> FieldString -> Bool
matches s field =
  normalize s == normalized field

renameUnnormalized :: (String -> Maybe String) -> (FieldString -> FieldString)
renameUnnormalized f input =
  fromMaybe input (f input)

normalize :: String -> String
normalize s =
  if all (not . isAllowedChar) s
    then s
    else
      slugify $
      dropWhile (== '-') $
      filter isAllowedChar $
      map (\ c -> if c == '_' then '-' else c) $
      s
  where
    slugify (a : r)
      | isUpper a = slugify (toLower a : r)
    slugify (a : b : r)
      | isUpper b = a : '-' : slugify (toLower b : r)
      | otherwise = a : slugify (b : r)
    slugify x = x

isAllowedChar :: Char -> Bool
isAllowedChar c = (isAscii c && isAlphaNum c) || (c `elem` "-_")
