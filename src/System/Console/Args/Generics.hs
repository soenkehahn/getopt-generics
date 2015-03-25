{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module System.Console.Args.Generics (withArguments) where

import           Control.Applicative
import           Data.List
import           Data.Monoid (Monoid, mempty)
import           Generics.SOP
import           Safe
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

withArguments :: forall a . (Generic a, HasDatatypeInfo a, All2 Option (Code a)) =>
  (a -> IO ()) -> IO ()
withArguments action = do
  args <- getArgs
  case parseArgs args of
    Right (Right a) -> action a
    Left noAction -> noAction
    Right (Left errors) -> do
      mapM_ (hPutStrLn stderr) errors
      exitWith $ ExitFailure 1

parseArgs :: forall a . (Generic a, HasDatatypeInfo a, All2 Option (Code a)) =>
  [String] -> Either (IO ()) (Either [String] a)
parseArgs args = case datatypeInfo (Proxy :: Proxy a) of
    ADT typeName _ (constructorInfo :* Nil) ->
      case constructorInfo of
        (Record _ fields) -> processFields args fields
        Constructor{} ->
          err typeName "constructors without field labels"
        Infix{} ->
          err typeName "infix constructors"
    ADT typeName _ Nil ->
      err typeName "empty data types"
    ADT typeName _ (_ :* _ :* _) ->
      err typeName "sum-types"
    Newtype _ _ (Record _ fields) ->
      processFields args fields
    Newtype typeName _ (Constructor _) ->
      err typeName "constructors without field labels"
  where
    err typeName message =
      Right $ Left ["args-generics doesn't support " ++ message ++ " (" ++ typeName ++ ")."]

processFields :: forall a xs . (Generic a, Code a ~ '[xs], SingI xs, All Option xs) =>
  [String] -> NP FieldInfo xs -> Either (IO ()) (Either [String] a)
processFields args fields =
  helpWrapper args fields $
  fmap (to . SOP . Z) $
  case getOpt Permute (mkOptDescrs fields) args of
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
  NP FieldInfo xs -> [OptDescr (NS FieldState xs)]
mkOptDescrs fields =
  map toOptDescr $ sumList $ npMap mkOptDescr fields

newtype OptDescrE a = OptDescrE (OptDescr (FieldState a))

mkOptDescr :: forall a . Option a => FieldInfo a -> OptDescrE a
mkOptDescr (FieldInfo name) = OptDescrE $ Option [] [name] toOption ""

toOptDescr :: NS OptDescrE xs -> OptDescr (NS FieldState xs)
toOptDescr (Z (OptDescrE a)) = fmap Z a
toOptDescr (S a) = fmap S (toOptDescr a)

mkEmptyArguments :: forall xs . (SingI xs, All Option xs) =>
  NP FieldInfo xs -> NP FieldState xs
mkEmptyArguments fields = case (sing :: Sing xs, fields) of
  (SNil, Nil) -> Nil
  (SCons, FieldInfo name :* r) ->
    emptyOption name :* mkEmptyArguments r
  _ -> error "mkEmpty: impossible"


-- * showing help?

helpWrapper :: (All Option xs) =>
  [String] -> NP FieldInfo xs -> a -> Either (IO ()) a
helpWrapper args fields a =
    case getOpt Permute [helpOption] args of
      ([], _, _) -> Right a
      (() : _, _, _) -> Left $ do
        progName <- getProgName
        let header = progName
        putStrLn (usageInfo header (mkOptDescrs fields))
  where
    helpOption = Option ['h'] ["help"] (NoArg ()) "show help and exit"


-- * helper functions for NS and NP

collectErrors :: NP FieldState xs -> Either [String] (NP I xs)
collectErrors np = case np of
  Nil -> Right Nil
  (a :* r) -> case (a, collectErrors r) of
    (Success a, Right r) -> Right (I a :* r)
    (ParseErrors errors, r) -> Left (errors ++ either id (const []) r)
    (Unset error, r) -> Left (error : either id (const []) r)
    (Success _, Left errors) -> Left errors

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
    inner Nil _ = error "project: impossible"


-- * possible field types

data FieldState a
  = Unset String
  | ParseErrors [String]
  | Success a
  deriving (Functor)

class Option a where
  toOption :: ArgDescr (FieldState a)
  emptyOption :: String -> FieldState a
  accumulate :: a -> a -> a
  accumulate _ x = x

combine :: Option a => FieldState a -> FieldState a -> FieldState a
combine _ (Unset _) = error "combine: shouldn't happen"
combine (ParseErrors e) (ParseErrors f) = ParseErrors (e ++ f)
combine (ParseErrors e) _ = ParseErrors e
combine (Unset _) x = x
combine (Success _) (ParseErrors e) = ParseErrors e
combine (Success a) (Success b) = Success (accumulate a b)

instance Option Bool where
  toOption = NoArg (Success True)
  emptyOption _ = Success False

instance Option String where
  toOption = ReqArg Success "string"
  emptyOption flagName = Unset
    ("missing option: --" ++ flagName ++ "=string")

instance Option (Maybe String) where
  toOption = ReqArg (Success . Just) "string (optional)"
  emptyOption _ = Success Nothing

instance Option [String] where
  toOption = ReqArg (Success . pure) "strings (multiple possible)"
  emptyOption _ = Success []
  accumulate = (++)

parseInt :: String -> FieldState Int
parseInt s = maybe (ParseErrors ["not an integer: " ++ s]) Success $ readMay s

instance Option Int where
  toOption = ReqArg parseInt "integer"
  emptyOption flagName = Unset
    ("missing option: --" ++ flagName ++ "=int")

instance Option (Maybe Int) where
  toOption = ReqArg (fmap Just . parseInt) "integer (optional)"
  emptyOption _ = Success Nothing
