{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE ViewPatterns     #-}

module System.Console.GetOpt.Generics.Modifier (
  Modifier(..),
  Modifiers,
  mkModifiers,
  mkShortOptions,
  mkLongOption,
  hasPositionalArgumentsField,
  isPositionalArgumentsField,
  getPositionalArgumentType,
  getHelpText,
  getVersion,

  deriveShortOptions,

  -- exported for testing
  mkShortModifiers,
  insertWith,
 ) where

import           Prelude ()
import           Prelude.Compat

import           Data.Char
import           Data.List (find, foldl')
import           Data.Maybe
import           Generics.SOP

import           System.Console.GetOpt.Generics.FieldString

-- | 'Modifier's can be used to customize the command line parser.
data Modifier
  = AddShortOption String Char
    -- ^ @AddShortOption fieldName c@ adds the 'Char' @c@ as a short option for
    --   the field addressed by @fieldName@.
  | RenameOption String String
    -- ^ @RenameOption fieldName customName@ renames the option generated
    --   through the @fieldName@ by @customName@.
  | RenameOptions (String -> Maybe String)
    -- ^ @RenameOptions f@ renames all options with the given functions. In case
    --   the function returns @Nothing@ the original field name is used.
    --
    --   Can be used together with 'Data.List.stripPrefix'.
  | UseForPositionalArguments String String
    -- ^ @UseForPositionalArguments fieldName argumentType@ fills the field
    --   addressed by @fieldName@ with the positional arguments (i.e. arguments
    --   that don't correspond to a flag). The field has to have type
    --   @['String']@.
    --
    --   @argumentType@ is used as the type of the positional arguments in the
    --   help output.
  | AddOptionHelp String String
    -- ^ @AddOptionHelp fieldName helpText@ adds a help text for the option
    --   @fieldName@.
  | AddVersionFlag String
    -- ^ @AddVersionFlag version@ adds a @--version@ flag.

data Modifiers = Modifiers {
  _shortOptions :: [(String, [Char])],
  _renaming :: FieldString -> FieldString,
  positionalArgumentsField :: [(String, String)],
  helpTexts :: [(String, String)],
  version :: Maybe String
 }

mkModifiers :: [Modifier] -> Modifiers
mkModifiers = foldl' inner empty
  where
    empty :: Modifiers
    empty = Modifiers [] id [] [] Nothing

    inner :: Modifiers -> Modifier -> Modifiers
    inner (Modifiers shorts renaming args help version) modifier = case modifier of
      (AddShortOption option short) ->
        Modifiers (insertWith (++) option [short] shorts) renaming args help version
      (RenameOption from to) ->
        let newRenaming :: FieldString -> FieldString
            newRenaming option = if from `matches` option
              then mkFieldString to
              else option
        in Modifiers shorts (renaming . newRenaming) args help version
      (RenameOptions newRenaming) ->
        Modifiers shorts (renaming `combineRenamings` newRenaming) args help version
      (UseForPositionalArguments option typ) ->
        Modifiers shorts renaming ((option, map toUpper typ) : args) help version
      (AddOptionHelp option helpText) ->
        Modifiers shorts renaming args (insert option helpText help) version
      (AddVersionFlag v) ->
        Modifiers shorts renaming args help (Just v)

    combineRenamings :: (FieldString -> FieldString) -> (String -> Maybe String)
      -> FieldString -> FieldString
    combineRenamings old new fieldString =
      (old . renameUnnormalized new) fieldString

lookupMatching :: [(String, a)] -> FieldString -> Maybe a
lookupMatching list option = fmap snd $ find (\ (from, _) -> from `matches` option) list

mkShortOptions :: Modifiers -> FieldString -> [Char]
mkShortOptions (Modifiers shortMap _ _ _ _) option = fromMaybe [] (lookupMatching shortMap option)

mkLongOption :: Modifiers -> FieldString -> String
mkLongOption (Modifiers _ renaming _ _ _) option =
  normalized (renaming option)

hasPositionalArgumentsField :: Modifiers -> Bool
hasPositionalArgumentsField = not . null . positionalArgumentsField

isPositionalArgumentsField :: Modifiers -> FieldString -> Bool
isPositionalArgumentsField modifiers field =
  any (`matches` field) (map fst (positionalArgumentsField modifiers))

getPositionalArgumentType :: Modifiers -> Maybe String
getPositionalArgumentType = fmap snd . listToMaybe . positionalArgumentsField

getHelpText :: Modifiers -> FieldString -> String
getHelpText modifiers field = fromMaybe "" $ lookupMatching (helpTexts modifiers) field

getVersion :: Modifiers -> Maybe String
getVersion modifiers = version modifiers

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
