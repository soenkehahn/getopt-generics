{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE ViewPatterns     #-}

module WithCli.Modifier (
  Modifier(..),
  Modifiers,
  mkModifiers,
  isPositionalArgumentsField,
  getPositionalArgumentType,
  getVersion,

  applyModifiers,
  applyModifiersLong,

  -- exported for testing
  insertWith,
 ) where

import           Prelude ()
import           Prelude.Compat

import           Control.Arrow
import           Control.Monad
import           Data.Char
import           Data.List (foldl')
import           Data.Maybe
import           System.Console.GetOpt

import           WithCli.Modifier.Types
import           WithCli.Normalize
import           WithCli.Parser
import           WithCli.Result

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
        let newRenaming :: String -> String
            newRenaming option = if from `matches` option
              then to
              else option
        in return $ Modifiers shorts (renaming . newRenaming) args help version
      (RenameOptions newRenaming) ->
        return $ Modifiers shorts (renaming `combineRenamings` newRenaming) args help version
      (UseForPositionalArguments option typ) -> case args of
        Nothing -> return $ Modifiers shorts renaming (Just (option, map toUpper typ)) help version
        Just _ -> Errors "UseForPositionalArguments can only be used once"
      (AddOptionHelp option helpText) ->
        return $ Modifiers shorts renaming args (insert option helpText help) version
      (AddVersionFlag v) ->
        return $ Modifiers shorts renaming args help (Just v)

    combineRenamings :: (a -> a) -> (a -> Maybe a) -> (a -> a)
    combineRenamings old new x = old (fromMaybe x (new x))

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

-- * transforming Parsers

applyModifiers :: Modifiers -> Parser Unnormalized a -> Parser Unnormalized a
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
