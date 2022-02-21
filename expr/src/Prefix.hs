module Prefix where

import Expr
import Data.Char ( isDigit, digitToInt )

-- Expr :: + Expr Expr
--       | * Expr Expr
--       | Digit
-- +1*234 -> Just ("4", ...)
parsePrefix :: String -> Maybe (String, Expr)
parsePrefix (op : t) | op `elem` "+-*/^" =
  case parsePrefix t of
    Just (t', l) ->
      case parsePrefix t' of
        Just (t'', r) -> Just (t'', BinOp (toOp op) l r)
        Nothing -> Nothing
    Nothing -> Nothing
parsePrefix (d : t) | isDigit d =
  Just (t, Num (digitToInt d))
parsePrefix _ = Nothing