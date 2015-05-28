{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
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

import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Proxy
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

-- | Pure variant of 'getArguments'.
--
--   Does not throw any exceptions.
parseArguments :: forall a . (Generic a, HasDatatypeInfo a, All2 Option (Code a)) =>
     String -- ^ Name of the program (e.g. from 'getProgName').
  -> [Modifier] -- ^ List of 'Modifier's to manually tweak the command line interface.
  -> [String] -- ^ List of command line arguments to parse (e.g. from 'getArgs').
  -> Result a
parseArguments progName modifiersList args = do
    (modifiers, datatypeInfo) <- (,) <$>
      mkModifiers modifiersList <*>
      normalizedDatatypeInfo (Proxy :: Proxy a)
    case datatypeInfo of
      ADT typeName _ (constructorInfo :* Nil) ->
        case constructorInfo of
          (Record _ fields) -> processFields progName modifiers args
            (hliftA (Comp . Selector) fields)
          Constructor{} ->
            processFields progName modifiers args (hpure (Comp NoSelector))
          Infix{} ->
            err typeName "infix constructors"
      ADT typeName _ Nil ->
        err typeName "empty data types"
      ADT typeName _ (_ :* _ :* _) ->
        err typeName "sum-types"
      Newtype _ _ (Record _ fields) ->
        processFields progName modifiers args
          (hliftA (Comp . Selector) fields)
      Newtype typeName _ (Constructor _) ->
        err typeName "constructors without field labels"
  where
    err typeName message =
      Errors ["getopt-generics doesn't support " ++ message ++
              " (" ++ typeName ++ ")."]

data Field a
  = NoSelector
  | Selector a

processFields :: forall a xs .
  (Generic a, Code a ~ '[xs], SingI xs, All Option xs) =>
  String -> Modifiers -> [String] -> NP (Field :.: FieldInfo) xs -> Result a
processFields progName modifiers args fields = do
    initialFieldStates <- mkInitialFieldStates modifiers fields

    showHelp

    let (options, arguments, parseErrors) =
          getOpt Permute (mkOptDescrs modifiers fields) args

    reportGetOptErrors parseErrors

    let (withPositionalArguments, additionalArgumentsErrors) =
          fillInPositionalArguments arguments $
            project options initialFieldStates
    either Errors return additionalArgumentsErrors

    to . SOP . Z <$> collectResult withPositionalArguments
  where
    showHelp :: Result ()
    showHelp = helpWrapper progName modifiers args fields

    reportGetOptErrors :: [String] -> Result ()
    reportGetOptErrors parseErrors = case parseErrors of
      [] -> pure ()
      errs -> Errors errs

mkOptDescrs :: forall xs . (SingI xs, All Option xs) =>
  Modifiers -> NP (Field :.: FieldInfo) xs -> [OptDescr (NS FieldState xs)]
mkOptDescrs modifiers =
  mapMaybe toOptDescr . apInjs_NP . hcliftA (Proxy :: Proxy Option) (mkOptDescr modifiers)

newtype OptDescrE a = OptDescrE (Maybe (OptDescr (FieldState a)))

mkOptDescr :: forall a . Option a => Modifiers -> (Field :.: FieldInfo) a -> OptDescrE a
mkOptDescr _modifiers (Comp NoSelector) = OptDescrE Nothing
mkOptDescr modifiers (Comp (Selector (FieldInfo name))) = OptDescrE $
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
  Modifiers -> NP (Field :.: FieldInfo) xs -> Result (NP FieldState xs)
mkInitialFieldStates modifiers fields = case (sing :: Sing xs, fields) of
  (SNil, Nil) -> return Nil
  (SCons, Comp (Selector (FieldInfo name)) :* r) ->
    (:*) <$> inner name <*> mkInitialFieldStates modifiers r
  (SCons, Comp NoSelector :* r) ->
    (:*) <$> Success PositionalArgument <*> mkInitialFieldStates modifiers r
  _ -> uninhabited "mkInitialFieldStates"

 where
  inner :: forall x . Option x => String -> Result (FieldState x)
  inner name = if isPositionalArgumentsField modifiers name
    then case cast (id :: FieldState x -> FieldState x) of
      (Just id' :: Maybe (FieldState [String] -> FieldState x)) ->
        Success $ id' PositionalArguments
      Nothing -> Errors
        ["UseForPositionalArguments can only be used " ++
         "for fields of type [String] not " ++
         show (typeOf (impossible "mkInitialFieldStates" :: x))]
    else return $ _emptyOption name


-- * showing help?

data HelpFlag = HelpFlag

helpWrapper :: (SingI xs, All Option xs) =>
  String -> Modifiers -> [String] -> NP (Field :.: FieldInfo) xs -> Result ()
helpWrapper progName modifiers args fields =
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

    header :: String
    header = unwords $
      progName :
      "[OPTIONS]" :
      positionalArgumentHelp fields ++
      maybe [] (\ t -> ["[" ++ map toUpper t ++ "]"])
        (getPositionalArgumentType modifiers) ++
      []

positionalArgumentHelp :: (All Option xs) => NP (Field :.: FieldInfo) xs -> [String]
positionalArgumentHelp (p@(Comp NoSelector) :* r) =
  map toUpper (argumentType (toProxy p)) : positionalArgumentHelp r
positionalArgumentHelp (_ :* r) = positionalArgumentHelp r
positionalArgumentHelp Nil = []

stripTrailingSpaces :: String -> String
stripTrailingSpaces = unlines . map stripLines . lines
  where
    stripLines = reverse . dropWhile isSpace . reverse

fillInPositionalArguments :: (All Option xs) =>
  [String] -> NP FieldState xs -> (NP FieldState xs, Either [String] ())
fillInPositionalArguments = inner . Just
  where
    inner :: All Option xs =>
      Maybe [String] -> NP FieldState xs -> (NP FieldState xs, Either [String] ())
    inner arguments fields = case (arguments, fields) of

      (Just arguments, PositionalArguments :* r) -> FieldSuccess arguments `cons` inner Nothing r
      (Nothing, PositionalArguments :* r) ->
        FieldErrors ["UseForPositionalArguments can only be used once"] `cons` inner Nothing r

      (Just (argument : arguments), PositionalArgument :* r) ->
        case parseArgumentEither argument of
          Right a -> FieldSuccess a `cons` inner (Just arguments) r
          Left err -> FieldErrors [err] `cons` inner (Just arguments) r
      (Just [], p@PositionalArgument :* r) ->
        FieldErrors ["missing argument of type " ++ map toUpper (argumentType (toProxy p))]
          `cons` inner (Just []) r
      (Nothing, PositionalArgument :* _) ->
        impossible "fillInPositionalArguments"

      (arguments, a :* r) -> a `cons` inner arguments r
      (Just [], Nil) -> (Nil, Right ())
      (Nothing, Nil) -> (Nil, Right ())
      (Just arguments@(_ : _), Nil) ->
        (Nil, Left (map (\ arg -> "unknown argument: " ++ arg) arguments))

    cons :: FieldState x -> (NP FieldState xs, r) -> (NP FieldState (x ': xs), r)
    cons fieldState (arguments, r) = (fieldState :* arguments, r)

collectResult :: (SingI xs) => NP FieldState xs -> Result (NP I xs)
collectResult input =
    hsequence $ hliftA inner input
  where
    inner :: FieldState x -> Result x
    inner s = case s of
      FieldSuccess v -> Success v
      FieldErrors errs -> Errors errs
      Unset err -> Errors [err]
      PositionalArguments -> impossible "collectResult"
      PositionalArgument -> impossible "collectResult"

-- * helper functions for NS and NP

project :: (SingI xs, All Option xs) =>
  [NS FieldState xs] -> NP FieldState xs -> NP FieldState xs
project sums start =
    foldl' inner start sums
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

toProxy :: f a -> Proxy a
toProxy = const Proxy

-- * possible field types

data FieldState a where
  Unset :: String -> FieldState a
  FieldErrors :: [String] -> FieldState a
  FieldSuccess :: a -> FieldState a
  PositionalArguments :: FieldState [String]
  PositionalArgument :: FieldState a
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

parseArgumentEither :: forall a . Option a => String -> Either String a
parseArgumentEither s =
  maybe
    (Left ("cannot parse as " ++ argumentType (Proxy :: Proxy a) ++ ": " ++ s ++ "\n"))
    Right
    (parseArgument s)

parseAsFieldState :: forall a . Option a => String -> FieldState a
parseAsFieldState s = either
  (\ err -> FieldErrors [err])
  FieldSuccess
  (parseArgumentEither s)

combine :: Option a => FieldState a -> FieldState a -> FieldState a
combine _ (Unset _) = impossible "combine"
combine _ PositionalArguments = impossible "combine"
combine _ PositionalArgument = impossible "combine"
combine (FieldErrors e) (FieldErrors f) = FieldErrors (e ++ f)
combine (FieldErrors e) _ = FieldErrors e
combine (Unset _) x = x
combine (FieldSuccess _) (FieldErrors e) = FieldErrors e
combine (FieldSuccess a) (FieldSuccess b) = FieldSuccess (_accumulate a b)
combine PositionalArguments _ = PositionalArguments
combine PositionalArgument _ = PositionalArgument

instance Option Bool where
  argumentType _ = "bool"

  parseArgument :: String -> Maybe Bool
  parseArgument s
    | map toLower s == "true" = Just True
    | map toLower s == "false" = Just False
    | otherwise = case readMaybe s of
      Just (n :: Integer) -> Just (n > 0)
      Nothing -> Nothing

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
