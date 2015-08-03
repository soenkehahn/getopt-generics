#!/usr/bin/env runhaskell

import           Control.DeepSeq
import           Data.List
import           System.IO
import           Text.Read (readMaybe)

main :: IO ()
main = do
  process "README.md"
  process "src/System/Console/GetOpt/Generics.hs"
  process "src/System/Console/GetOpt/Generics/GetArguments.hs"
  process "src/SimpleCLI.hs"

process :: FilePath -> IO ()
process file = do
  hPutStrLn stderr ("processing " ++ file)
  contents <- strictReadFile file
  output <- fillInHoles contents
  deepseq output (return ())
  writeFile file output

fillInHoles :: String -> IO String
fillInHoles contents = do
  let lines = parseLines contents
  renderLines <$> fillInLines lines

data Line
  = String String
  | Instruction String Instruction

isEndInstruction :: Line -> Bool
isEndInstruction (Instruction _ End) = True
isEndInstruction _ = False

data Instruction
  = Start FilePath FileType
  | End
  deriving (Read)

data FileType
  = MarkDown CodeType
  | Haddock
  deriving (Read)

data CodeType
  = Shell
  | Haskell
  deriving (Read)

renderCodeType :: CodeType -> String
renderCodeType Shell = "shell"
renderCodeType Haskell = "haskell"

parseLines :: String -> [Line]
parseLines = map parse . lines'
  where
  parse :: String -> Line
  parse s
    | "###" `isInfixOf` s =
      let instruction = unwords $ takeWhile (/= "###") $ drop 2 $ words s in
      case (readMaybe instruction :: Maybe Instruction) of
        Nothing -> error ("cannot parse: " ++ instruction)
        Just x -> Instruction s x
    | otherwise = String s

renderLines :: [Line] -> String
renderLines = concat . map (\ (String s) -> s)

fillInLines :: [Line] -> IO [Line]
fillInLines lines = case lines of
  [] -> return []
  (String s : rest) -> (String s :) <$> fillInLines rest
  (Instruction startLine (Start file fileType) : rest) -> do
    hPutStrLn stderr ("splicing in " ++ file)
    fileContent <- readFile file
    let (Instruction endLine End : afterEnd) =
          dropWhile (not . isEndInstruction) rest
    filledInAfterEnd <- fillInLines afterEnd
    return $
      String startLine :
      render fileType startLine fileContent endLine ++
      String endLine :
      filledInAfterEnd
 where
  render (MarkDown codeType) startLine fileContent endLine =
    String ("\n``` " ++ renderCodeType codeType ++ "\n") :
    String fileContent :
    String "```\n\n" :
    []
  render Haddock startLine fileContent endLine =
    String "\n-- |\n" :
    String (concat $ map addHaddockCodeMarker $
      lines' fileContent) :
    String "\n" :
    []

addHaddockCodeMarker :: String -> String
addHaddockCodeMarker "\n" = "-- >\n"
addHaddockCodeMarker s = "-- >  " ++ s

strictReadFile :: FilePath -> IO String
strictReadFile file = do
  r <- readFile file
  deepseq r (return ())
  return r

lines' :: String -> [String]
lines' [] = []
lines' s = case span (/= '\n') s of
  (line, '\n' : rest) -> (line ++ "\n") : lines' rest
  (s, []) -> [s]
