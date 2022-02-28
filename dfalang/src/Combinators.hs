{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Combinators where

import Control.Applicative

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
  fmap = (<*>) . pure

instance Applicative Parser where
  pure res = Parser $ \str -> pure (str, res)

  p1 <*> p2 = Parser $ \s -> do
    (s', f) <- runParser p1 s
    (s'', x) <- runParser p2 s'
    return (s'', f x)

instance Alternative Parser where
  empty = Parser $ const empty

  p1 <|> p2 = Parser $ \str -> runParser p1 str <|> runParser p2 str

instance Monad Parser where
  return = pure

  f >>= p = Parser $ \str -> do
    (str', res) <- runParser f str
    runParser (p res) str'

instance MonadFail Parser where
  fail = Parser . (const fail)



