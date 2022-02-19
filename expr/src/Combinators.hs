module Combinators where
import Data.Bifunctor (Bifunctor(second))
import Expr ( Expr(Num) )
import Data.Char ( isDigit, digitToInt )

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

-- first [(sep, second), (sep', third)] -> sep' (sep (first, second)) third

leftAssoc :: (elem -> sep -> elem -> elem) -> (elem, [(sep, elem)]) -> elem
leftAssoc f (first, rest) =
  foldl (\acc (sep, elem) -> f acc sep elem) first rest

rightAssoc :: (elem -> sep -> elem -> elem) -> ([(elem, sep)], elem) -> elem
rightAssoc f (first, rest) =
    foldr (\(elem, sep) acc -> f elem sep acc) rest first 


list :: Parser elem -> Parser sep -> Parser (elem, [(sep, elem)])
list elem sep = do
    first <- elem
    rest <- goParser <|> return []
    return (first, rest)
  where
    goParser = do
      second <- sep 
      third <- elem
      rest' <- goParser <|> return []
      return $ (second, third): rest'


listR :: Parser elem -> Parser sep -> Parser ([(elem, sep)], elem)
listR elem sep = do
    all <- goParser <|> return []
    last <- elem
    return (all, last)
  where
    goParser = do
      first <- elem 
      second <- sep
      rest' <- goParser <|> return []
      return $ (first, second): rest'

(<|>) :: Parser a -> Parser a -> Parser a
l <|> r = Parser $ \str ->
  case runParser l str of
    Just (str', res) -> Just (str', res)
    Nothing -> runParser r str

instance Functor Parser where
  fmap f p = Parser $ \str ->
    case runParser p str of
      Just (s', res) -> Just (s', f res)
      Nothing -> Nothing

instance Applicative Parser where
  pure a = Parser $ \str -> Just (str,a)
  f <*> g = Parser $ \str -> case runParser f str of
    Nothing -> Nothing 
    Just(str', h) -> case runParser g str of
      Nothing -> Nothing 
      Just(str'', x) -> Just(str'', h x)

instance Monad Parser where
  (>>=) l r = Parser $ \str ->
    case runParser l str of
      Just (str', res) -> runParser (r res) str'
      Nothing -> Nothing

char :: Char -> Parser Char
char c = Parser $ \str ->
  case str of
    (h : t) | c == h -> Just (t, c)
    _ -> Nothing


parseDigit :: Parser Expr
parseDigit = Parser $ \str ->
  case str of
    (d : t) | isDigit d -> Just (t, Num (digitToInt d))
    _ -> Nothing