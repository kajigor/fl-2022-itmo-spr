module Combinators where

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

-- first [(sep, second), (sep', third)] -> sep' (sep (first, second)) third

leftAssoc :: (elem -> sep -> elem -> elem) -> (elem, [(sep, elem)]) -> elem
leftAssoc f (first, rest) =
  foldl (\acc (sep, elem) -> f acc sep elem) first rest

rightAssoc :: (elem -> sep -> elem -> elem) -> ([(elem, sep)], elem) -> elem
rightAssoc f (beginning, last) =
  foldr (\(elem, sep) acc -> f elem sep acc) last beginning

-- Expr :: Expr - Expr | Expr + Expr (Левоассоциативно)
--       | Expr * Expr | Expr / Expr (Левоассоциативно)
--       | Expr ^ Expr               (Правоассоциативно)
--       | Digit
--       | ( Expr )

-- Expr (op Expr) (op Expr) ... (op Expr) -> (Expr, [(op, Expr)])
list :: Parser elem -> Parser sep -> Parser (elem, [(sep, elem)])
list elem sep = do
    first <- elem
    rest <- goParser
    return $ (first, rest)


    -- Parser $ \str ->
    --   case runParser elem str of
    --     Just (str', first) ->
    --       case go str' of
    --         Just (str'', rest) -> Just (str'', (first, rest))
    --         Nothing -> Just (str', (first, []))
    --     Nothing -> Nothing
  where
    goParser = Parser go
    go str =
      case runParser sep str of
        Just (str', sep') ->
          case runParser elem str' of
            Just (str'', elem') ->
              case go str'' of
                Just (str''', res) ->
                  Just (str''', (sep', elem') : res)
                Nothing ->
                  Just (str'', [(sep', elem')])
            Nothing -> Nothing
        Nothing -> Just (str, [])

listR :: Parser elem -> Parser sep -> Parser ([(elem, sep)], elem)
listR elem sep = do
  first <- elem
  (do 
    sep' <- sep
    (lst, last) <- listR elem sep
    return ((first, sep'):lst, last)
   ) <|> return ([], first)

-- andThen == >>=
andThen :: Parser a1 -> (a1 -> Parser a2) -> Parser a2
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
  fmap = (<*>) . pure

instance Applicative Parser where
  pure x = Parser $ \s -> Just (s, x)

  (Parser p1) <*> (Parser p2) = Parser $ \s -> do
    (s', f) <- p1 s
    (s'', x) <- p2 s'
    return (s'', f x)

instance Monad Parser where
  (>>=) = andThen
