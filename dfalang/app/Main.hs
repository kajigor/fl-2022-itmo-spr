module Main where

import Parser
import DFA
import Util (orElse)

import Data.Maybe (fromMaybe)
import Options.Applicative
import System.FilePath ((<.>))
import Text.Printf (printf)

data ConsumeAction = NoCheck | CheckFile FilePath | CheckStr String
            deriving (Show)

data Action = Action { input :: FilePath, dumpToStdout :: Bool, consume :: ConsumeAction }
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

consumeFileParser :: Parser ConsumeAction
consumeFileParser = CheckFile <$> strOption
  (  short 'f'
  <> metavar "LANG_FILE"
  <> help "File to run against DFA" )

consumeStringParser :: Parser ConsumeAction
consumeStringParser = CheckStr <$> argument str
  (  metavar "LANG_STR"
  <> help "String to run against DFA" )

actionParser :: Parser Action
actionParser =
  Action <$> inputParser <*> dumpToStdoutParser <*> (consumeFileParser <|> consumeStringParser <|> pure NoCheck)

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

getCheckInput :: ConsumeAction -> IO (Maybe String)
getCheckInput NoCheck = pure Nothing
getCheckInput (CheckFile path) = pure <$> readFile path
getCheckInput (CheckStr s) = pure $ pure s

runParser :: String -> FilePath -> IO (Maybe DFA)
runParser s path =
  case parse s of
    Just dfa -> do
      writeFile path $ printf "%s\n" $ prettyPrint dfa
      return $ Just dfa
    Nothing -> do
      writeFile path $ "Syntax error."
      return Nothing

dumpIntoStdout :: Bool -> String -> FilePath -> IO ()
dumpIntoStdout False _ _ = return ()
dumpIntoStdout True i o = do
  out <- readFile o
  putStrLn $ printf "===================================\nInput:\n\n%s\n-----------------------------------\nOutput:\n\n%s\n===================================\n" i out

runCheck :: DFA -> String -> FilePath -> IO ()
runCheck dfa str path = do
  let consumed = checkConsume dfa $ map (Sym . (:[])) str
  if consumed
  then appendFile path $ printf "\n\n>>> String '%s' was successfully cosumed\n" str
  else appendFile path $ printf "\n\n>>> Failed to consume '%s' by your DFA\n" str

runAction :: Action -> IO ()
runAction (Action input dump checkAction) = do
  i <- readFile input
  let o = input <.> "out"
  mbDfa <- runParser i o
  mbCheckStr <- getCheckInput checkAction
  orElse (runCheck <$> mbDfa <*> mbCheckStr <*> pure o) (pure ())
  dumpIntoStdout dump i o

