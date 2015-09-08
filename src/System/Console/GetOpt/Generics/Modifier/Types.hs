
module System.Console.GetOpt.Generics.Modifier.Types where

data Modifiers = Modifiers {
  shortOptions :: [(String, [Char])],
  renaming :: String -> String,
  positionalArgumentsField :: Maybe (String, String),
  _helpTexts :: [(String, String)],
  version :: Maybe String
 }

getVersion :: Modifiers -> Maybe String
getVersion modifiers = version modifiers

isPositionalArgumentsField :: Modifiers -> String -> Bool
isPositionalArgumentsField modifiers field =
  maybe False ((field ==) . fst) (positionalArgumentsField modifiers)

getPositionalArgumentType :: Modifiers -> Maybe String
getPositionalArgumentType = fmap snd . positionalArgumentsField
