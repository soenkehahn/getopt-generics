{-# LANGUAGE GADTs #-}

module System.Console.GetOpt.Generics.Hint (
  Hint(..),
  defaultHints,
  slugify,
  mkShortOptions,
 ) where

import           Data.Char
import           Data.List
import           Data.Maybe
import           Generics.SOP

data Hint
  = Short String Char
  deriving (Show, Eq, Ord)

defaultHints :: HasDatatypeInfo a =>
  Proxy a -> [Hint]
defaultHints proxy =
  mkShortHints (flags proxy)

flags :: HasDatatypeInfo a =>
  Proxy a -> [String]
flags proxy = case datatypeInfo proxy of
    ADT _ _ ci -> fromNPConstructorInfo ci
    Newtype _ _ ci -> fromConstructorInfo ci
  where
    fromNPConstructorInfo :: NP ConstructorInfo xs -> [String]
    fromNPConstructorInfo Nil = []
    fromNPConstructorInfo (a :* r) =
      fromConstructorInfo a ++ fromNPConstructorInfo r

    fromConstructorInfo :: ConstructorInfo x -> [String]
    fromConstructorInfo (Constructor _) = []
    fromConstructorInfo (Infix _ _ _) = []
    fromConstructorInfo (Record _ fields) =
      fromFields fields

    fromFields :: NP FieldInfo xs -> [String]
    fromFields (FieldInfo name :* r) = name : fromFields r
    fromFields Nil = []

mkShortHints :: [String] -> [Hint]
mkShortHints fields =
    catMaybes $ map inner fields
  where
    inner :: String -> Maybe Hint
    inner field@(short : _) =
      case filter ([short] `isPrefixOf`) fields of
        [_] -> Just $ Short field short
        _ -> Nothing
    inner [] = Nothing

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
