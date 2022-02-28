module Parser 
  ( parse
  ) where

import DFA
import Combinators
import Util
import Control.Applicative
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char ( isDigit, isLetter )

parse :: String -> Maybe DFA
parse str = snd <$> runParser dfaSpec str

dfaSpec :: Parser DFA
dfaSpec = do
  mbAlphabet <- optional alphabetDecl
  mbStates <- optional statesDecl
  start <- startDecl
  mbTerminals <- optional terminalsDecl
  transitions <- many transitionDecl
  transitionsMap <- transitionsToMap transitions
  
  let dfa = DFA start (orElse mbTerminals S.empty) transitionsMap
  let actualStates = dfaStates dfa
  let actualAlphabet = dfaAlphabet dfa
 
  if maybe True (actualStates `S.isSubsetOf`) mbStates
  then pure ()
  else fail "States declaration is controversial"

  if maybe True (actualAlphabet `S.isSubsetOf`) mbAlphabet
  then pure ()
  else fail "Alphabet declaration is controversial"

  return dfa
 where
  assertOnlyDupValues :: (Ord v, MonadFail m) => M.Map k [v] -> m (M.Map k v)
  assertOnlyDupValues m = do
    let mSet = M.map S.fromList m
    let withNotDups = M.filter (\s -> S.size s /= 1) mSet
    
    if M.null withNotDups
    then pure ()
    else fail "Controversial transitions declarations"

    return $ M.map (S.elemAt 0) mSet

  transitionsToMap :: (MonadFail m) => [(State, State, S.Set Symbol)] -> m (M.Map State (M.Map Symbol State))
  transitionsToMap transitions = do
    let stMapList = associate (\(st1, sym, st2) -> (st1, (sym, st2))) $ triples transitions
    let stSymMapList = M.map (associate id) stMapList
    mapM assertOnlyDupValues stSymMapList
    
  triples :: [(State, State, S.Set Symbol)] -> [(State, Symbol, State)]
  triples transitions = do
    (st1, st2, syms) <- transitions
    map (\sym -> (st1, sym, st2)) $ S.toList syms

alphabetDecl :: Parser (S.Set Symbol)
alphabetDecl = decl $ do
  spaced $ word "Alphabet"
  colon
  S.map Sym <$> nameSet

statesDecl :: Parser (S.Set State)
statesDecl = decl $ do
  spaced $ word "States"
  colon
  S.map St <$> nameSet

startDecl :: Parser State
startDecl = decl $ do
  spaced $ word "Start"
  colon
  St <$> name 

terminalsDecl :: Parser (S.Set State)
terminalsDecl = decl $ do
  spaced $ word "Terminals"
  colon
  S.map St <$> nameSet

transitionDecl :: Parser (State, State, S.Set Symbol)
transitionDecl = decl $ do
  st1 <- St <$> name 
  arrow
  st2 <- St <$> name
  colon
  symbols <- S.map Sym <$> nameSet
  return (st1, st2, symbols)

decl :: Parser a -> Parser a
decl p = many declSep *> p <* many declSep

declSep :: Parser Char
declSep = char ';' <|> char '\n'

colon :: Parser Char
colon = spaced $ char ':'

arrow :: Parser String
arrow = spaced $ word "->"

nameSet :: Parser (S.Set String)
nameSet = S.fromList <$> many name

name :: Parser String
name = spaced $ some $ satisfy (\c -> isDigit c || isLetter c || c == '_') item

word :: String -> Parser String
word s = reverse <$> (foldl prepend (pure []) $ map char s)
  where 
    prepend :: Parser [a] -> Parser a -> Parser [a]
    prepend acc ch = do 
      res <- acc
      res' <- ch
      return $ res' : res
 

spaced :: Parser a -> Parser a
spaced p = many spaceOrTab *> p <* many spaceOrTab

spaceOrTab = char ' ' <|> char '\t'

char :: Char -> Parser Char
char c = satisfy (c==) item

satisfy :: (a -> Bool) -> Parser a -> Parser a
satisfy p parser = do
  res <- parser
  if p res
  then return res
  else fail "Predicate failed"

item :: Parser Char
item = Parser $ \str ->
  case str of
    (h : t) -> return (t, h)
    _ -> fail "EOF reached"

