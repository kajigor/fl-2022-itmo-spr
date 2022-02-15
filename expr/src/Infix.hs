{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
module Infix where
import           Combinators                    ( Parser(..)
                                                , list
                                                , parseDigit
                                                , parseDiv
                                                , parseHat
                                                , parseMinus
                                                , parsePlus
                                                , parseStar
                                                )
import           Expr                           ( Expr(BinOp)
                                                , Operator
                                                )
import           GHC.Base                       ( (<|>) )

-- Expr :: Expr - Expr | Expr + Expr (Левоассоциативно)
--       | Expr * Expr | Expr / Expr (Левоассоциативно)
--       | Expr ^ Expr               (Правоассоциативно)
--       | Digit
--       | ( Expr )
-- Expr :: Слаг + Слаг + ... + Слаг
-- Слаг :: Множ (* Множ) * ... (* Множ) -> [Expr]
-- Множ :: Цифра | Выражение в скобках
parseInfix :: String -> Maybe (String, Expr)
parseInfix = runParser parseSum

parseSum :: Parser Expr
parseSum = leftAssoc toBinOp <$> list parseMult (parsePlus <|> parseMinus)

parseMult :: Parser Expr
parseMult = leftAssoc toBinOp <$> list parsePow (parseStar <|> parseDiv)

parsePow :: Parser Expr
parsePow = rightAssoc toBinOp <$> list (parseDigit <|> parseExprBr) parseHat

toBinOp :: Expr -> Operator -> Expr -> Expr
toBinOp l op = BinOp op l

parseExprBr :: Parser Expr
parseExprBr = Parser $ \case
  ('(' : t) -> case runParser parseSum t of
    Just (')' : t', e) -> Just (t', e)
    _                  -> Nothing
  _ -> Nothing

-- first [(sep, second), (sep', third)] -> sep' (sep (first, second)) third

leftAssoc :: (elem -> sep -> elem -> elem) -> (elem, [(sep, elem)]) -> elem
leftAssoc f (first, rest) =
  foldl (\acc (sep, elem) -> f acc sep elem) first rest

rightAssoc :: (elem -> sep -> elem -> elem) -> (elem, [(sep, elem)]) -> elem
rightAssoc f (first, rest) =
  let (beginning, last) = go (first, rest)
  in  foldr (\(elem, sep) acc -> f elem sep acc) last beginning
 where
  go :: (elem, [(sep, elem)]) -> ([(elem, sep)], elem)
  go (first, []) = ([], first)
  go (first, (sep, second) : rest) =
    let (list, last) = go (second, rest) in ((first, sep) : list, last)
