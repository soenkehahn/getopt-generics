
module System.Console.GetOpt.Generics.Modifier.Types where

data Modifiers = Modifiers {
  shortOptions :: [(String, [Char])],
  renaming :: String -> String,
  positionalArgumentsField :: Maybe (String, String),
  helpTexts :: [(String, String)],
  version :: Maybe String
 }

getVersion :: Modifiers -> Maybe String
getVersion modifiers = version modifiers

hasPositionalArgumentsField :: Modifiers -> Bool
hasPositionalArgumentsField = not . null . positionalArgumentsField

isPositionalArgumentsField :: Modifiers -> String -> Bool
isPositionalArgumentsField modifiers field =
  maybe False ((field ==) . fst) (positionalArgumentsField modifiers)

getPositionalArgumentType :: Modifiers -> Maybe String
getPositionalArgumentType = fmap snd . positionalArgumentsField
