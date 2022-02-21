{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Combinators where

import Control.Applicative
import Data.Char ( isSpace )

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
  fmap f p =  Parser (fmap (f <$>) . runParser p)

instance Applicative Parser where
  pure res = Parser $ \str -> Just (str, res)
  (<*>) f p = Parser $ \str ->
    case runParser f str of
      Just (str', f') ->
        case runParser p str' of
          Just (str'', a) -> Just (str'', f' a)
          Nothing -> Nothing
      Nothing -> Nothing

instance Alternative Parser where
  empty = Parser $ const Nothing
  (<|>) l r = Parser $ \str ->
    case runParser l str of
      Just (str', res) -> Just (str', res)
      Nothing -> runParser r str

instance Monad Parser where
  return = pure
  (>>=) f p = Parser $ \str ->
    case runParser f str of
      Just (str', res) -> runParser (p res) str'
      Nothing -> Nothing

instance MonadFail Parser where
  fail _ = Parser $ const Nothing

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
    go (first, (sep, second) : rest) =
      let (list, last) = go (second, rest) in
      ((first, sep) : list, last)

list :: Parser elem -> Parser sep -> Parser (elem, [(sep, elem)])
list elem sep = do
      first <- elem
      rest <- many go
      return (first, rest)
  where
    go = do
      sep' <- sep
      elem' <- elem
      return (sep', elem')

char :: Char -> Parser Char
char c = Parser $ \str ->
  case str of
    (h : t) | c == h -> Just (t, c)
    _ -> Nothing
