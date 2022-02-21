module ListR where

import Combinators
import Control.Applicative


-- list :: Parser elem -> Parser sep -> Parser (elem, [(sep, elem)])
-- list elem sep = do
--       first <- elem
--       rest <- many go
--       return (first, rest)
--   where
--     go = do
--       sep' <- sep
--       elem' <- elem
--       return (sep', elem')

-- "a,b,c,"
listR :: Parser elem -> Parser sep -> Parser ([(elem, sep)], elem)
listR elem sep = do
    first <- many go
    last <- elem
    return (first, last)
  where
    go = do
      elem <- elem
      sep <- sep
      return (elem, sep)



-- Should work, but does not
-- > runParser (listR (a <|> b <|> c) zpt) "a,b,c,"
-- > Nothing
listRFaulty :: Parser elem -> Parser sep -> Parser ([(elem, sep)], elem)
listRFaulty elem sep = do
  head <- goParser <|> return []
  last <- elem
  return (head, last)
  where
    goParser = do
      elem' <- elem
      sep' <- sep
      res <- goParser <|> return []
      return ((elem', sep') : res)

-- (e s) (e s) (e s) (e s) e
-- e (s e) (s e) (s e)...

-- works
listR' :: Parser elem -> Parser sep -> Parser ([(elem, sep)], elem)
listR' elem sep = do
  first <- elem
  (do
    sep' <- sep
    (lst, last) <- listR' elem sep
    return ((first, sep'):lst, last)
   ) <|> return ([], first)

-- also works
listR'' :: Parser expr -> Parser sep -> Parser ([(expr, sep)], expr)
listR'' expr sep = do
  firstElem <- expr
  goParser firstElem <|> return ([], firstElem)
  where
    goParser element = do
      sep' <- sep
      expr' <- expr
      (res, lastElem) <- goParser expr' <|> return ([], expr')
      return ((element, sep') : res, lastElem)



a = char 'a'
b = char 'b'
c = char 'c'
zpt = char ','
parse p = runParser (p (a <|> b <|> c) zpt)
























-- > runParser (listR (a <|> b <|> c) zpt) "a,b,c,"
-- > Just (",",([('a',','),('b',',')],'c'))
