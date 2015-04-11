{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TupleSections    #-}

module System.Console.GetOpt.Generics.Modifier (
  Modifier(..),
  Modifiers,
  mkModifiers,
  mkShortOptions,
  mkLongOption,

  deriveShortOptions,

  -- exported for testing
  mkShortModifiers,
  insertWith,
 ) where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Generics.SOP

import           System.Console.GetOpt.Generics.Internal
import           System.Console.GetOpt.Generics.Result

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
  _shortOptions :: [(String, [Char])],
  _renamings :: [(String, String)]
 }

mkModifiers :: [Modifier] -> Result Modifiers
mkModifiers = foldM inner (Modifiers [] [])
  where
    inner :: Modifiers -> Modifier -> Result Modifiers
    inner (Modifiers shorts renamings) (AddShortOption option short) = do
      normalized <- normalizeFieldName option
      return $ Modifiers
        (insertWith (++) normalized [short] shorts)
        renamings
    inner (Modifiers shorts renamings) (RenameOption from to) = do
      fromNormalized <- normalizeFieldName from
      return $ Modifiers shorts (insert fromNormalized to renamings)

mkShortOptions :: Modifiers -> String -> [Char]
mkShortOptions (Modifiers shortMap _) option =
    fromMaybe [] (lookup option shortMap)

mkLongOption :: Modifiers -> String -> String
mkLongOption (Modifiers _ renamings) option =
  fromMaybe option (lookup option renamings)

-- * deriving Modifiers

-- | Derives 'AddShortOption's for all fields of the datatype that start with a
--   unique character.
deriveShortOptions :: (HasDatatypeInfo a, SingI (Code a)) =>
  Proxy a -> [Modifier]
deriveShortOptions proxy =
  mkShortModifiers (flags proxy)

flags :: (SingI (Code a), HasDatatypeInfo a) =>
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

mkShortModifiers :: [String] -> [Modifier]
mkShortModifiers fields =
    let withShorts = mapMaybe (\ field -> (field, ) <$> toShort field) fields
        allShorts = map snd withShorts
        isUnique c = case filter (== c) allShorts of
          [_] -> True
          _ -> False
    in (flip mapMaybe) withShorts $ \ (field, short) ->
          if isUnique short
            then Just (AddShortOption field short)
            else Nothing
  where
    toShort :: String -> Maybe Char
    toShort s = case dropWhile (\ c -> not (isAscii c && isAlpha c)) s of
      [] -> Nothing
      (a : _) -> Just (toLower a)

-- * list utils to replace Data.Map

insertWith :: Eq a => (b -> b -> b) -> a -> b -> [(a, b)] -> [(a, b)]
insertWith _ key value [] = [(key, value)]
insertWith combine key value ((a, b) : r) =
  if a == key
    then (key, b `combine` value) : r
    else (a, b) : insertWith combine key value r

insert :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
insert key value [] = [(key, value)]
insert key value ((a, b) : r) =
  if a == key
    then (key, value) : r
    else (a, b) : insert key value r
