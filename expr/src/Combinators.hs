module Combinators where
import Data.Maybe (isNothing)

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

-- first [(sep, second), (sep', third)] -> sep' (sep (first, second)) third

leftAssoc :: (elem -> sep -> elem -> elem) -> (elem, [(sep, elem)]) -> elem
leftAssoc f (first, rest) =
  foldl (\acc (sep, elem) -> f acc sep elem) first rest


rightAssoc :: (elem -> sep -> elem -> elem) -> (elem, [(sep, elem)]) -> elem
rightAssoc f (first, rest) =
    let (beginning, last) = go (first, rest) in
    foldr (\(elem, sep) acc -> f elem sep acc) last beginning
  where
    go :: (elem, [(sep, elem)]) -> ([(elem, sep)], elem)
    go (first, []) = ([], first)
    go (first, ((sep, second) : rest)) =
      let (list, last) = go (second, rest) in
      ((first, sep) : list, last)

-- Expr :: Expr - Expr | Expr + Expr (Левоассоциативно)
--       | Expr * Expr | Expr / Expr (Левоассоциативно)
--       | Expr ^ Expr               (Правоассоциативно)
--       | Digit
--       | ( Expr )

-- Expr (op Expr) (op Expr) ... (op Expr) -> (Expr, [(op, Expr)])
list :: Parser elem -> Parser sep -> Parser (elem, [(sep, elem)])
list elem sep = do 
    first <- elem
    rest <- goParser <|> pure []
    return (first ,rest)
    where 
      goParser = do
        sep' <- sep 
        expr' <- elem
        res <- goParser <|> pure []
        return ([(sep', expr')] ++ res)

-- andThen == >>=
andThen l r = Parser $ \str ->
  case runParser l str of
    Just (str', res) -> runParser (r res) str'
    Nothing -> Nothing


char :: Char -> Parser Char
char c = Parser $ \str ->
  case str of
    (h : t) | c == h -> Just (t, c)
    _ -> Nothing

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
  pure x = Parser $ \str -> Just (str, x)
  pleft <*> pright = Parser $ \str ->  case runParser pleft str of
    Nothing -> Nothing
    Just (str', y) -> case runParser pright str of
      Just (str'', m) -> Just (str'', y m)    
      _ -> Nothing

instance Monad Parser where
  return x = Parser $ \str -> Just (str, x)
  m >>= k = Parser $ \str -> case runParser m str of
    Just (str', l) -> runParser (k l) str'
    _ -> Nothing
