{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module System.Console.GetOpt.Generics.Modifier (
  Modifier(..),
  deriveShortOptions,
  mkShortOptions,
  mkLongOptions,
 ) where

import           Data.List
import           Data.Maybe
import           Generics.SOP

import           System.Console.GetOpt.Generics.Internal

data Modifier
  = AddShortOption String Char
  | RenameOption String String
  deriving (Show, Eq, Ord)

deriveShortOptions :: (HasDatatypeInfo a, SingI (Code a)) =>
  Proxy a -> [Modifier]
deriveShortOptions proxy =
  mkShortModifiers (flags proxy)

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

mkShortModifiers :: [String] -> [Modifier]
mkShortModifiers fields =
    mapMaybe inner fields
  where
    inner :: String -> Maybe Modifier
    inner field@(short : _) =
      case filter ([short] `isPrefixOf`) fields of
        [_] -> Just $ AddShortOption field short
        _ -> Nothing
    inner [] = Nothing

mkShortOptions :: [Modifier] -> String -> [Char]
mkShortOptions modifiers option =
    mapMaybe inner modifiers
  where
    inner :: Modifier -> Maybe Char
    inner (AddShortOption modifierOption short)
      | matchesField modifierOption option
        = Just short
      | otherwise
        = Nothing
    inner _ = Nothing

mkLongOptions :: [Modifier] -> String -> [String]
mkLongOptions modifiers option =
    inner (reverse modifiers)
  where
    inner (RenameOption renameOption newName : _)
      | renameOption `matchesField` option = [newName]
    inner [] = [option]
    inner (_ : r) = inner r

matchesField :: String -> String -> Bool
matchesField modifierOption option =
  modifierOption == option || slugify modifierOption == option
