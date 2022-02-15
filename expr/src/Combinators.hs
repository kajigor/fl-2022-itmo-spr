{-# LANGUAGE LambdaCase #-}
module Combinators where
import           Data.Char                      ( digitToInt
                                                , isDigit
                                                )
import           Expr                           ( Expr(Num)
                                                , Operator
                                                  ( Div
                                                  , Minus
                                                  , Mult
                                                  , Plus
                                                  , Pow
                                                  )
                                                )
import           GHC.Base                       ( (<|>)
                                                , Alternative(empty)
                                                )

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

-- Expr :: Expr - Expr | Expr + Expr (Левоассоциативно)
--       | Expr * Expr | Expr / Expr (Левоассоциативно)
--       | Expr ^ Expr               (Правоассоциативно)
--       | Digit
--       | ( Expr )

-- Expr (op Expr) (op Expr) ... (op Expr) -> (Expr, [(op, Expr)])
list :: Parser elem -> Parser sep -> Parser (elem, [(sep, elem)])
list elem sep = do
  first <- elem
  rest  <- goParser
  return (first, rest)
 where
  goParser = Parser go
  go str = case runParser sep str of
    Just (str', sep') -> case runParser elem str' of
      Just (str'', elem') -> case go str'' of
        Just (str''', res) -> Just (str''', (sep', elem') : res)
        Nothing            -> Just (str'', [(sep', elem')])
      Nothing -> Nothing
    Nothing -> Just (str, [])

char :: Char -> Parser Char
char c = Parser $ \case
  (h : t) | c == h -> Just (t, c)
  _                -> Nothing
instance Functor Parser where
  fmap f p = Parser $ \str -> case runParser p str of
    Just (s', res) -> Just (s', f res)
    Nothing        -> Nothing

instance Applicative Parser where
  pure a = Parser (\s -> Just (s, a))
  Parser f <*> Parser a = Parser $ \s -> case f s of
    Just (s', f') -> fmap f' <$> a s'
    Nothing       -> Nothing

instance Alternative Parser where
  empty = Parser (const Nothing)
  l <|> r = Parser $ \str -> case runParser l str of
    Just (str', res) -> Just (str', res)
    Nothing          -> runParser r str

instance Monad Parser where
  l >>= r = Parser $ \str -> case runParser l str of
    Just (str', res) -> runParser (r res) str'
    Nothing          -> Nothing

parsePlus :: Parser Operator
parsePlus = Plus <$ char '+'

parseMinus :: Parser Operator
parseMinus = Minus <$ char '-'

parseStar :: Parser Operator
parseStar = Mult <$ char '*'

parseDiv :: Parser Operator
parseDiv = Div <$ char '/'

parseHat :: Parser Operator
parseHat = Pow <$ char '^'

parseOperator :: Parser Operator
parseOperator =
  parsePlus <|> parseMinus <|> parseStar <|> parseDiv <|> parseHat

parseDigit :: Parser Expr
parseDigit = Parser $ \case
  (d : t) | isDigit d -> Just (t, Num (digitToInt d))
  _                   -> Nothing
