{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module System.Console.GetOpt.Generics.Hint (
  Hint(..),
  deriveShortOptions,
  mkShortOptions,
  mkLongOptions,
 ) where

import           Data.List
import           Data.Maybe
import           Generics.SOP

import           System.Console.GetOpt.Generics.Internal

data Hint
  = Short String Char
  | RenameOption String String
  deriving (Show, Eq, Ord)

deriveShortOptions :: (HasDatatypeInfo a, SingI (Code a)) =>
  Proxy a -> [Hint]
deriveShortOptions proxy =
  mkShortHints (flags proxy)

flags :: (SingI (Code a), HasDatatypeInfo a) =>
  Proxy a -> [String]
flags proxy = case normalizedDatatypeInfo proxy of
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

mkShortOptions :: [Hint] -> String -> [Char]
mkShortOptions hints option =
    catMaybes $ map inner hints
  where
    inner :: Hint -> Maybe Char
    inner (Short hintOption short)
      | matchesField hintOption option
        = Just short
      | otherwise
        = Nothing
    inner _ = Nothing

mkLongOptions :: [Hint] -> String -> [String]
mkLongOptions hints option =
    inner (reverse hints)
  where
    inner (RenameOption renameOption newName : _)
      | renameOption `matchesField` option = [newName]
    inner [] = [option]
    inner (_ : r) = inner r

matchesField :: String -> String -> Bool
matchesField hintOption option =
  hintOption == option || slugify hintOption == option
