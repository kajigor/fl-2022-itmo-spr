module Combinators where
import Control.Applicative (Alternative ((<|>), empty))
import Data.Char ( isDigit, digitToInt )

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

leftAssoc :: (elem -> sep -> elem -> elem) -> (elem, [(sep, elem)]) -> elem
leftAssoc f (first, rest) =
  foldl (\acc (sep, elem) -> f acc sep elem) first rest

rightAssoc :: (elem -> sep -> elem -> elem) -> ([(elem, sep)], elem) -> elem
rightAssoc f (beginning, last) =
    foldr (\(elem, sep) acc -> f elem sep acc) last beginning

listR :: Parser elem -> Parser sep -> Parser ([(elem, sep)], elem)
listR elem sep = do
          beginnig <- goParser
          last <- elem
          return (beginnig, last)
  where
    goParser = (do 
        elem' <- elem
        sep' <- sep
        rest <- goParser
        return ((elem', sep'):rest))  <|> return []
      
list :: Parser elem -> Parser sep -> Parser (elem, [(sep, elem)])
list elem sep = do
          first <- elem
          rest <- goParser
          return (first, rest)
  where
    revert x = Parser (\s -> g s (runParser x s))
      where
        g s Nothing = Just (s,[])
        g _ (Just _) = Nothing

    goParser = revert sep <|> do
                    sep' <- sep
                    elem' <- elem
                    rest <- goParser
                    return ((sep', elem'):rest)

char :: Char -> Parser Char
char c = Parser h
  where
    h (k:s') | k == c = Just (s',k)
    h _ = Nothing

instance Functor Parser where
  fmap f p = 
    let 
      g x = return (f <$> x)
      h s = runParser p s >>= g
     in Parser h

instance Applicative Parser where
  pure a = Parser (\s -> return (s,a))
  mf <*> p = Parser(\s -> do
                        (s',f) <- runParser mf s
                        (s'',x) <- runParser p s'
                        Just (s'', f x))

instance Monad Parser where
  return = pure
  p >>= f = 
    let 
      g (s', e) = runParser (f e) s'
      h s = runParser p s >>= g
    in Parser h

instance Alternative Parser where 
  empty = Parser (const Nothing)
  l <|> r = Parser (\s -> runParser l s <|> runParser r s)

