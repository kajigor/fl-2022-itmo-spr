{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Parser
  ( object
  , Parser(runParser)
  ) where

import           Control.Applicative            ( (<|>)
                                                , Alternative(empty, many, some)
                                                )
import           Data.Char                      ( digitToInt
                                                , isDigit
                                                )
import qualified Data.Map                      as M
import           JSON                           ( JSON
                                                  ( Array
                                                  , Number
                                                  , Object
                                                  , Str
                                                  )
                                                )

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ (fmap . fmap) f . p

instance Applicative Parser where
  pure a = Parser $ Just . (, a)
  Parser pf <*> Parser pa = Parser $ \s -> case pf s of
    Nothing      -> Nothing
    Just (s', f) -> (fmap . fmap . fmap) f pa s'

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser l <|> Parser r = Parser $ \s -> case l s of
    Nothing -> r s
    Just a  -> Just a

instance Monad Parser where
  Parser pa >>= f = Parser $ \s -> case pa s of
    Nothing      -> Nothing
    Just (s', a) -> runParser (f a) s'

forbiddenChars :: [Char]
forbiddenChars = "\"\\"

space :: Parser Char
space = char (== ' ')

lineBreak :: Parser Char
lineBreak = char (== '\n')

whitespace :: Parser [Char]
whitespace = many $ space <|> lineBreak

allowedChar :: Parser Char
allowedChar = char (`notElem` forbiddenChars)

escapedForbiddenChar :: Parser Char
escapedForbiddenChar = do
  char (== '\\')
  char (`elem` forbiddenChars)

char :: (Char -> Bool) -> Parser Char
char p = Parser $ \case
  (c : rest) | p c -> Just (rest, c)
  _                -> Nothing

strToInt :: String -> Int
strToInt s = go $ reverse s
 where
  go []         = 0
  go (c : rest) = 10 * go rest + digitToInt c

number :: Parser JSON
number = do
  digits <- some $ char isDigit
  if head digits /= '0' || length digits == 1
    then return $ Number $ strToInt digits
    else empty

string :: Parser String
string = do
  char (== '"')
  s <- many $ allowedChar <|> escapedForbiddenChar
  char (== '"')
  return s

array :: Parser JSON
array = do
  char (== '[')
  (do
      char (== ']')
      return $ Array []
    )
    <|> (do
          whitespace
          fst  <- json
          rest <- many
            (do
              whitespace
              char (== ',')
              whitespace
              json
            )
          whitespace
          char (== ']')
          return $ Array (fst : rest)
        )

field :: Parser (String, JSON)
field = do
  name <- string
  many space
  char (== ':')
  many space
  body <- json
  return (name, body)

object :: Parser JSON
object = do
  char (== '{')
  (do
      char (== '}')
      return $ Object M.empty
    )
    <|> (do
          whitespace
          fst  <- field
          rest <- many
            (do
              whitespace
              char (== ',')
              whitespace
              field
            )
          whitespace
          char (== '}')
          return $ Object $ M.fromList (fst : rest)
        )

json :: Parser JSON
json = number <|> Str <$> string <|> array <|> object
