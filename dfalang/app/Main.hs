module Main where

import Parser
import DFA

import Data.Maybe (fromMaybe)
import Options.Applicative
import System.FilePath ((<.>))
import Text.Printf (printf)


data Action = Action { input :: FilePath, dumpToStdout :: Bool }
            deriving (Show)

inputParser :: Parser FilePath
inputParser = strOption
  (  short 'i'
  <> metavar "INPUT"
  <> help "Input file" )

dumpToStdoutParser :: Parser Bool
dumpToStdoutParser = flag False True
  (  short 'd'
  <> help "Render input and output into stdout"
  )

actionParser :: Parser Action
actionParser =
  Action <$> inputParser <*> dumpToStdoutParser

main :: IO ()
main = do
  runAction =<< execParser opts
  where
    opts = info (actionParser <**> helper)
      (  fullDesc
      <> progDesc "Parser for DFA specification language"
      <> header "DFAlang parser"
      )

getOutput :: FilePath -> IO FilePath
getOutput path = return $ path <.> "out"

defaultOutFile = "str.out"

runParser :: String -> FilePath -> IO ()
runParser s path =
  case parse s of
    Just dfa ->
      writeFile path $ printf "%s\n" $ prettyPrint dfa
    Nothing ->
      writeFile path $ "Syntax error."

dumpIntoStdout :: Bool -> String -> FilePath -> IO ()
dumpIntoStdout False _ _ = return ()
dumpIntoStdout True i o = do
  out <- readFile o
  putStrLn $ printf "===================================\nInput:\n\n%s\n-----------------------------------\nOutput:\n\n%s\n===================================\n" i out

runAction :: Action -> IO ()
runAction (Action input dump) = do
  i <- readFile input
  let o = input <.> "out"
  runParser i o
  dumpIntoStdout dump i o
