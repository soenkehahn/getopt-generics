{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fno-warn-deprecated-flags #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}

-- | @getopt-generics@ tries to make it very simple to create command line
-- argument parsers. An introductory example can be found in the
-- <https://github.com/zalora/getopt-generics#getopt-generics README>.

module System.Console.GetOpt.Generics (
  -- * IO API
  getArguments,
  modifiedGetArguments,
  -- * Pure API
  parseArguments,
  Result(..),
  -- * Customizing the CLI
  Modifier(..),
  deriveShortOptions,
  -- * Available Field Types
  Option(..),
  -- * Re-exports from "Generics.SOP"
  Generic,
  HasDatatypeInfo,
  Proxy(..)
 ) where

import           Data.Orphans ()
import           Prelude ()
import           Prelude.Compat

import           Control.Monad (when)
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Typeable
import           Generics.SOP
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Read.Compat

import           System.Console.GetOpt.Generics.Internal
import           System.Console.GetOpt.Generics.Modifier
import           System.Console.GetOpt.Generics.Result

-- | Parses command line arguments (gotten from 'withArgs') and returns the
--   parsed value. This function should be enough for simple use-cases.
--
--   May throw the following exceptions:
--
--   - @'ExitFailure' 1@ in case of invalid options. Error messages are written
--     to @stderr@.
--   - @'ExitSuccess'@ in case @--help@ is given. (@'ExitSuccess'@ behaves like
--     a normal exception, except that -- if uncaught -- the process will exit
--     with exit-code @0@.) Help output is written to @stdout@.
getArguments :: forall a . (Generic a, HasDatatypeInfo a, All2 Option (Code a)) =>
  IO a
getArguments = modifiedGetArguments []

-- | Like 'getArguments` but allows you to pass in 'Modifier's.
modifiedGetArguments :: forall a . (Generic a, HasDatatypeInfo a, All2 Option (Code a)) =>
  [Modifier] -> IO a
modifiedGetArguments modifiers = do
  args <- getArgs
  progName <- getProgName
  case parseArguments progName modifiers args of
    Success a -> return a
    OutputAndExit message -> do
      putStr message
      exitWith ExitSuccess
    Errors errs -> do
      mapM_ (hPutStr stderr) errs
      exitWith $ ExitFailure 1

-- | Pure variant of 'getArguments'. Also allows to declare 'Modifier's.
--
--   Does not throw any exceptions.
parseArguments :: forall a . (Generic a, HasDatatypeInfo a, All2 Option (Code a)) =>
  String -> [Modifier] -> [String] -> Result a
parseArguments header modifiersList args = do
    (modifiers, datatypeInfo) <- (,) <$>
      mkModifiers modifiersList <*>
      normalizedDatatypeInfo (Proxy :: Proxy a)
    case datatypeInfo of
      ADT typeName _ (constructorInfo :* Nil) ->
        case constructorInfo of
          (Record _ fields) -> processFields header modifiers args fields
          Constructor{} ->
            err typeName "constructors without field labels"
          Infix{} ->
            err typeName "infix constructors"
      ADT typeName _ Nil ->
        err typeName "empty data types"
      ADT typeName _ (_ :* _ :* _) ->
        err typeName "sum-types"
      Newtype _ _ (Record _ fields) ->
        processFields header modifiers args fields
      Newtype typeName _ (Constructor _) ->
        err typeName "constructors without field labels"
  where
    err typeName message =
      Errors ["getopt-generics doesn't support " ++ message ++
              " (" ++ typeName ++ ")."]

processFields :: forall a xs .
  (Generic a, Code a ~ '[xs], SingI xs, All Option xs) =>
  String -> Modifiers -> [String] -> NP FieldInfo xs -> Result a
processFields header modifiers args fields =
    mkInitialFieldStates modifiers fields >>= \ initialFieldStates ->

    showHelp *>

    let (options, arguments, parseErrors) =
          getOpt Permute (mkOptDescrs modifiers fields) args
    in

    reportParseErrors parseErrors *>

    reportInvalidPositionalArguments arguments *>

    produceResult initialFieldStates options arguments
  where
    showHelp :: Result ()
    showHelp = helpWrapper header modifiers args fields

    reportParseErrors :: [String] -> Result ()
    reportParseErrors parseErrors = case parseErrors of
      [] -> pure ()
      errs -> Errors errs

    reportInvalidPositionalArguments :: [String] -> Result ()
    reportInvalidPositionalArguments arguments =
      when (not $ hasPositionalArgumentsField modifiers) $
        case arguments of
          [] -> pure ()
          _ -> Errors (map ("unknown argument: " ++) arguments)

    produceResult :: NP FieldState xs -> [NS FieldState xs] -> [String] -> Result a
    produceResult initialFieldStates options arguments =
      (to . SOP . Z) <$>
        collectResult arguments (project options initialFieldStates)


mkOptDescrs :: forall xs . (SingI xs, All Option xs) =>
  Modifiers -> NP FieldInfo xs -> [OptDescr (NS FieldState xs)]
mkOptDescrs modifiers =
  mapMaybe toOptDescr . apInjs_NP . hcliftA (Proxy :: Proxy Option) (mkOptDescr modifiers)

newtype OptDescrE a = OptDescrE (Maybe (OptDescr (FieldState a)))

mkOptDescr :: forall a . Option a => Modifiers -> FieldInfo a -> OptDescrE a
mkOptDescr modifiers (FieldInfo name) = OptDescrE $
  if isPositionalArgumentsField modifiers name
    then Nothing
    else Just $ Option
      (mkShortOptions modifiers name)
      [mkLongOption modifiers name]
      _toOption
      (getHelpText modifiers name)

toOptDescr :: NS OptDescrE xs -> Maybe (OptDescr (NS FieldState xs))
toOptDescr (Z (OptDescrE (Just a))) = Just $ fmap Z a
toOptDescr (Z (OptDescrE Nothing)) = Nothing
toOptDescr (S a) = fmap (fmap S) (toOptDescr a)

mkInitialFieldStates :: forall xs . (SingI xs, All Option xs) =>
  Modifiers -> NP FieldInfo xs -> Result (NP FieldState xs)
mkInitialFieldStates modifiers fields = case (sing :: Sing xs, fields) of
  (SNil, Nil) -> return Nil
  (SCons, FieldInfo name :* r) ->
    (:*) <$> inner name <*> mkInitialFieldStates modifiers r
  _ -> uninhabited "mkEmpty"

 where
  inner :: forall x . Option x => String -> Result (FieldState x)
  inner name = if isPositionalArgumentsField modifiers name
    then case cast (id :: FieldState x -> FieldState x) of
      (Just id' :: Maybe (FieldState [String] -> FieldState x)) ->
        return $ id' PositionalArguments
      Nothing -> Errors
        ["UseForPositionalArguments can only be used " ++
         "for fields of type [String] not " ++
         show (typeOf (impossible "mkInitialFieldStates" :: x))]
    else return $ _emptyOption name


-- * showing help?

data HelpFlag = HelpFlag

helpWrapper :: (SingI xs, All Option xs) =>
  String -> Modifiers -> [String] -> NP FieldInfo xs -> Result ()
helpWrapper header modifiers args fields =
    case getOpt Permute [helpOption] args of
      ([], _, _) -> return ()
        -- no help flag given
      (HelpFlag : _, _, _) -> OutputAndExit $
        stripTrailingSpaces $
        usageInfo header $
          toOptDescrUnit (mkOptDescrs modifiers fields) ++
          toOptDescrUnit [helpOption]
  where
    helpOption :: OptDescr HelpFlag
    helpOption = Option ['h'] ["help"] (NoArg HelpFlag) "show help and exit"

    toOptDescrUnit :: [OptDescr a] -> [OptDescr ()]
    toOptDescrUnit = map (fmap (const ()))

stripTrailingSpaces :: String -> String
stripTrailingSpaces = unlines . map stripLines . lines
  where
    stripLines = reverse . dropWhile isSpace . reverse

-- * helper functions for NS and NP

collectResult :: SingI xs => [String] -> NP FieldState xs -> Result (NP I xs)
collectResult positionalArguments = hsequence . hliftA inner
  where
    inner :: FieldState a -> Result a
    inner (FieldSuccess v) = Success v
    inner (ParseErrors errs) = Errors errs
    inner (Unset err) = Errors [err]
    inner PositionalArguments = Success positionalArguments

project :: (SingI xs, All Option xs) =>
  [NS FieldState xs] -> NP FieldState xs -> NP FieldState xs
project sums empty =
    foldl' inner empty sums
  where
    inner :: (All Option xs) =>
      NP FieldState xs -> NS FieldState xs -> NP FieldState xs
    inner (a :* r) (Z b) = combine a b :* r
    inner (a :* r) (S rSum) = a :* inner r rSum
    inner Nil _ = uninhabited "project"

impossible :: String -> a
impossible name = error ("System.Console.GetOpt.Generics." ++ name ++ ": This should never happen!")

uninhabited :: String -> a
uninhabited = impossible

-- * possible field types

data FieldState a where
  Unset :: String -> FieldState a
  ParseErrors :: [String] -> FieldState a
  FieldSuccess :: a -> FieldState a
  PositionalArguments :: FieldState [String]
  deriving (Typeable)

-- | Type class for all allowed field types.
--
--   If you want to use custom field types you should implement an
--   @instance Option YourCustomType@ containing implementations of
--   'argumentType' and 'parseArgument' (the minimal complete definition). For
--   an example see the
--   <https://github.com/zalora/getopt-generics#getopt-generics README>.
class Typeable a => Option a where
  {-# MINIMAL argumentType, parseArgument #-}
  -- | Name of the argument type, e.g. "bool" or "integer".
  argumentType :: Proxy a -> String

  -- | Parses a 'String' into an argument. Returns 'Nothing' on parse errors.
  parseArgument :: String -> Maybe a

  -- | This is meant to be an internal function.
  _toOption :: ArgDescr (FieldState a)
  _toOption = ReqArg parseAsFieldState (argumentType (Proxy :: Proxy a))

  -- | This is meant to be an internal function.
  _emptyOption :: String -> FieldState a
  _emptyOption flagName = Unset
    ("missing option: --" ++ flagName ++ "=" ++ argumentType (Proxy :: Proxy a) ++ "\n")

  -- | This is meant to be an internal function.
  _accumulate :: a -> a -> a
  _accumulate _ x = x

parseAsFieldState :: forall a . Option a => String -> FieldState a
parseAsFieldState s = case parseArgument s of
  Just a -> FieldSuccess a
  Nothing -> ParseErrors $ pure $
    "cannot parse as " ++ argumentType (Proxy :: Proxy a) ++ ": " ++ s ++ "\n"

combine :: Option a => FieldState a -> FieldState a -> FieldState a
combine _ (Unset _) = impossible "combine"
combine _ PositionalArguments = impossible "combine"
combine (ParseErrors e) (ParseErrors f) = ParseErrors (e ++ f)
combine (ParseErrors e) _ = ParseErrors e
combine (Unset _) x = x
combine (FieldSuccess _) (ParseErrors e) = ParseErrors e
combine (FieldSuccess a) (FieldSuccess b) = FieldSuccess (_accumulate a b)
combine PositionalArguments _ = PositionalArguments

instance Option Bool where
  argumentType _ = "bool"
  parseArgument = impossible "Option.Bool.parseArguments"

  _toOption = NoArg (FieldSuccess True)
  _emptyOption _ = FieldSuccess False

instance Option a => Option [a] where
  argumentType Proxy = argumentType (Proxy :: Proxy a) ++ " (multiple possible)"
  parseArgument x = case parseArgument x of
    Just (x :: a) -> Just [x]
    Nothing -> Nothing
  _emptyOption _ = FieldSuccess []
  _accumulate = (++)

instance Option a => Option (Maybe a) where
  argumentType Proxy = argumentType (Proxy :: Proxy a) ++ " (optional)"
  parseArgument x = case parseArgument x of
    Just (x :: a) -> Just (Just x)
    Nothing -> Nothing
  _emptyOption _ = FieldSuccess Nothing

instance Option String where
  argumentType Proxy = "string"
  parseArgument = Just

instance Option Int where
  argumentType _ = "integer"
  parseArgument = readMaybe
