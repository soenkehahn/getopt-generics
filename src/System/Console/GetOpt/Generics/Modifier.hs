{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module System.Console.GetOpt.Generics.Modifier (
  Modifier(..),
  Modifiers,
  mkModifiers,
  mkShortOptions,
  mkLongOption,

  deriveShortOptions,
 ) where

import           Data.List
import qualified Data.Map.Strict                                as M
import           Data.Maybe
import           Generics.SOP

import           System.Console.GetOpt.Generics.Internal

-- | 'Modifier's can be used to customize the command line parser.
data Modifier
  = AddShortOption String Char
    -- ^ @AddShortOption fieldName c@ adds the 'Char' @c@ as a short option for
    --   the field addressed by @fieldName@.
  | RenameOption String String
    -- ^ @RenameOption fieldName customName@ renames the option generated
    --   through the @fieldName@ by @customName@.
  deriving (Show, Eq, Ord)

data Modifiers = Modifiers {
  _shortOptions :: M.Map String [Char],
  _renamings :: M.Map String String
 }

mkModifiers :: [Modifier] -> Modifiers
mkModifiers = foldl' inner (Modifiers M.empty M.empty)
  where
    inner :: Modifiers -> Modifier -> Modifiers
    inner (Modifiers shorts renamings) (AddShortOption option short) =
      Modifiers
        (M.insertWith (++) (slugify option) [short] shorts)
        renamings
    inner (Modifiers shorts renamings) (RenameOption from to) =
      Modifiers shorts (M.insert (slugify from) to renamings)

mkShortOptions :: Modifiers -> String -> [Char]
mkShortOptions (Modifiers shortMap _) option =
    fromMaybe [] (M.lookup option shortMap)

mkLongOption :: Modifiers -> String -> String
mkLongOption (Modifiers _ renamings) option =
  fromMaybe option (M.lookup option renamings)

-- * deriving Modifiers

-- | Derives 'AddShortOption's for all fields of the datatype that start with a
--   unique character.
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
