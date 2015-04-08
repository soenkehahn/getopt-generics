{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}

-- | "getopt-generics" tries to make it very simple to create command line
-- argument parsers. Documentation can be found in the
-- <https://github.com/zalora/getopt-generics#getopt-generics README>.

module System.Console.GetOpt.Generics (
  getArguments,
  parseArguments,
  Result(..),
  Modifier(..),
  deriveShortOptions,
  Option(..),
 ) where

import           Prelude ()
import           Prelude.Compat

import           Data.Char
import           Data.List
import           Generics.SOP
import           System.Console.GetOpt.Compat
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Read.Compat

import           System.Console.GetOpt.Generics.Modifier
import           System.Console.GetOpt.Generics.Internal

getArguments :: forall a . (Generic a, HasDatatypeInfo a, All2 Option (Code a)) =>
  IO a
getArguments = do
  args <- getArgs
  progName <- getProgName
  case parseArguments progName [] args of
    Success a -> return a
    OutputAndExit message -> do
      putStrLn message
      exitWith ExitSuccess
    Errors errs -> do
      mapM_ (hPutStrLn stderr) errs
      exitWith $ ExitFailure 1

data Result a
  = Success a
  | OutputAndExit String
  | Errors [String]
  deriving (Show, Eq, Ord)

parseArguments :: forall a . (Generic a, HasDatatypeInfo a, All2 Option (Code a)) =>
  String -> [Modifier] -> [String] -> Result a
parseArguments header modifiers args = case normalizedDatatypeInfo (Proxy :: Proxy a) of
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
      Errors ["getopt-generics doesn't support " ++ message ++ " (" ++ typeName ++ ")."]

processFields :: forall a xs . (Generic a, Code a ~ '[xs], SingI xs, All Option xs) =>
  String -> [Modifier] -> [String] -> NP FieldInfo xs -> Result a
processFields header modifiers args fields =
  helpWrapper header modifiers args fields $
  fmap (to . SOP . Z) $
  case getOpt Permute (mkOptDescrs modifiers fields) args of
    (options, arguments, parseErrors) ->
      let result :: Either [String] (NP I xs) =
            collectErrors $ project options (mkEmptyArguments fields)
          allErrors =
            parseErrors ++
            map mkUnknownArgumentError arguments ++
            ignoreRight result
      in case allErrors of
        [] -> result
        _ -> Left allErrors
  where
    mkUnknownArgumentError :: String -> String
    mkUnknownArgumentError arg = "unknown argument: " ++ arg

    ignoreRight :: Monoid e => Either e o -> e
    ignoreRight = either id (const mempty)

mkOptDescrs :: forall xs . All Option xs =>
  [Modifier] -> NP FieldInfo xs -> [OptDescr (NS FieldState xs)]
mkOptDescrs modifiers fields =
  map toOptDescr $ sumList $ npMap (mkOptDescr modifiers) fields

newtype OptDescrE a = OptDescrE (OptDescr (FieldState a))

mkOptDescr :: forall a . Option a => [Modifier] -> FieldInfo a -> OptDescrE a
mkOptDescr modifiers (FieldInfo name) = OptDescrE $
  Option
    (mkShortOptions modifiers name)
    (mkLongOptions modifiers name)
    toOption
    ""

toOptDescr :: NS OptDescrE xs -> OptDescr (NS FieldState xs)
toOptDescr (Z (OptDescrE a)) = fmap Z a
toOptDescr (S a) = fmap S (toOptDescr a)

mkEmptyArguments :: forall xs . (SingI xs, All Option xs) =>
  NP FieldInfo xs -> NP FieldState xs
mkEmptyArguments fields = case (sing :: Sing xs, fields) of
  (SNil, Nil) -> Nil
  (SCons, FieldInfo name :* r) ->
    emptyOption name :* mkEmptyArguments r
  _ -> uninhabited "mkEmpty"


-- * showing help?

data HelpFlag = HelpFlag

helpWrapper :: (All Option xs) =>
  String -> [Modifier] -> [String] -> NP FieldInfo xs -> Either [String] a -> Result a
helpWrapper header modifiers args fields result =
    case getOpt Permute [helpOption] args of
      ([], _, _) -> case result of
        -- no help flag given
        Left errs -> Errors errs
        Right a -> Success a
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

collectErrors :: NP FieldState xs -> Either [String] (NP I xs)
collectErrors np = case np of
  Nil -> Right Nil
  (a :* r) -> case (a, collectErrors r) of
    (FieldSuccess a, Right r) -> Right (I a :* r)
    (ParseErrors errs, r) -> Left (errs ++ either id (const []) r)
    (Unset err, r) -> Left (err : either id (const []) r)
    (FieldSuccess _, Left errs) -> Left errs

npMap :: (All Option xs) => (forall a . Option a => f a -> g a) -> NP f xs -> NP g xs
npMap _ Nil = Nil
npMap f (a :* r) = f a :* npMap f r

sumList :: NP f xs -> [NS f xs]
sumList Nil = []
sumList (a :* r) = Z a : map S (sumList r)

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

data FieldState a
  = Unset String
  | ParseErrors [String]
  | FieldSuccess a
  deriving (Functor)

class Option a where
  {-# MINIMAL argumentType, parseArgument #-}
  argumentType :: Proxy a -> String

  parseArgument :: String -> Maybe a

  toOption :: ArgDescr (FieldState a)
  toOption = ReqArg parseAsFieldState (argumentType (Proxy :: Proxy a))

  emptyOption :: String -> FieldState a
  emptyOption flagName = Unset
    ("missing option: --" ++ flagName ++ "=" ++ argumentType (Proxy :: Proxy a))

  accumulate :: a -> a -> a
  accumulate _ x = x

parseAsFieldState :: forall a . Option a => String -> FieldState a
parseAsFieldState s = case parseArgument s of
  Just a -> FieldSuccess a
  Nothing -> ParseErrors $ pure $
    "cannot parse as " ++ argumentType (Proxy :: Proxy a) ++ ": " ++ s

combine :: Option a => FieldState a -> FieldState a -> FieldState a
combine _ (Unset _) = impossible "combine"
combine (ParseErrors e) (ParseErrors f) = ParseErrors (e ++ f)
combine (ParseErrors e) _ = ParseErrors e
combine (Unset _) x = x
combine (FieldSuccess _) (ParseErrors e) = ParseErrors e
combine (FieldSuccess a) (FieldSuccess b) = FieldSuccess (accumulate a b)

instance Option Bool where
  argumentType _ = "bool"
  parseArgument = impossible "Option.Bool.parseArguments"

  toOption = NoArg (FieldSuccess True)
  emptyOption _ = FieldSuccess False

instance Option String where
  argumentType _ = "string"
  parseArgument = Just

instance Option (Maybe String) where
  argumentType _ = "string (optional)"
  parseArgument = Just . Just
  emptyOption _ = FieldSuccess Nothing

instance Option [String] where
  argumentType _ = "string (multiple possible)"
  parseArgument = Just . pure
  emptyOption _ = FieldSuccess []
  accumulate = (++)

instance Option Int where
  argumentType _ = "integer"
  parseArgument = readMaybe

instance Option (Maybe Int) where
  argumentType _ = "integer (optional)"
  parseArgument s = case readMaybe s of
    Just i -> Just (Just i)
    Nothing -> Nothing
  emptyOption _ = FieldSuccess Nothing

instance Option [Int] where
  argumentType _ = "integer (multiple possible)"
  parseArgument s = case readMaybe s of
    Just a -> Just [a]
    Nothing -> Nothing
  emptyOption _ = FieldSuccess []
  accumulate = (++)
