{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE ViewPatterns     #-}

module System.Console.GetOpt.Generics.Modifier (
  Modifier(..),
  Modifiers,
  mkModifiers,
  isPositionalArgumentsField,
  getPositionalArgumentType,
  getVersion,

  deriveShortOptions,

  applyModifiers,
  applyModifiersLong,

  -- exported for testing
  mkShortModifiers,
  insertWith,
 ) where

import           Prelude ()
import           Prelude.Compat

import           Control.Arrow
import           Control.Monad
import           Data.Char
import           Data.List (foldl')
import           Data.Maybe
import           Generics.SOP
import           System.Console.GetOpt

import           SimpleCLI.FromArguments
import           SimpleCLI.Result
import           System.Console.GetOpt.Generics.FieldString
import           System.Console.GetOpt.Generics.Modifier.Types

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

-- fixme: dcd

mkModifiers :: [Modifier] -> Result Modifiers
mkModifiers = foldM inner empty
  where
    empty :: Modifiers
    empty = Modifiers [] id Nothing [] Nothing

    inner :: Modifiers -> Modifier -> Result Modifiers
    inner (Modifiers shorts renaming args help version) modifier = case modifier of
      (AddShortOption option short) ->
        return $ Modifiers (insertWith (++) option [short] shorts) renaming args help version
      (RenameOption from to) ->
        let newRenaming :: FieldString -> FieldString
            newRenaming option = if from `matches` option
              then mkFieldString to
              else option
        in return $ Modifiers shorts (renaming . newRenaming) args help version
      (RenameOptions newRenaming) ->
        return $ Modifiers shorts (renaming `combineRenamings` newRenaming) args help version
      (UseForPositionalArguments option typ) -> case args of
        Nothing -> return $ Modifiers shorts renaming (Just (option, map toUpper typ)) help version
        Just _ -> Errors ["UseForPositionalArguments can only be used once"]
      (AddOptionHelp option helpText) ->
        return $ Modifiers shorts renaming args (insert option helpText help) version
      (AddVersionFlag v) ->
        return $ Modifiers shorts renaming args help (Just v)

    combineRenamings :: (FieldString -> FieldString) -> (String -> Maybe String)
      -> FieldString -> FieldString
    combineRenamings old new fieldString =
      (old . renameUnnormalized new) fieldString

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

-- * transforming FromArguments

applyModifiers :: Modifiers -> FromArguments Unnormalized a -> FromArguments Unnormalized a
applyModifiers modifiers =
  addShortOptions >>>
  renameOptions
  where
    addShortOptions = modParserOptions $ map $
      \ option ->
        case filter (\ (needle, _) -> needle `elem` longs option) (shortOptions modifiers) of
          [] -> option
          (concat . map snd -> newShorts) ->
            foldl' (flip addShort) option newShorts
    renameOptions =
      modParserOptions $ map $ modLongs $ renaming modifiers

applyModifiersLong :: Modifiers -> String -> String
applyModifiersLong modifiers long = (renaming modifiers) long

longs :: OptDescr a -> [String]
longs (Option _ ls _ _) = ls

addShort :: Char -> OptDescr a -> OptDescr a
addShort short (Option shorts longs argDescrs help) =
  Option (shorts ++ [short]) longs argDescrs help

modLongs :: (String -> String) -> OptDescr a -> OptDescr a
modLongs f (Option shorts longs descrs help) =
  Option shorts (map f longs) descrs help

-- fixme: tests for conflicting flags
-- todo: test for overlapping modifiers
