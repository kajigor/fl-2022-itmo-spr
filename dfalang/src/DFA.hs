module DFA where

import Util ( associate )
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (intercalate)
import Text.Printf ( printf )

newtype Symbol = Sym { symName :: String } deriving (Eq, Ord, Show)
newtype State = St { stName :: String } deriving (Eq, Ord, Show)

data DFA = DFA {
  dfaStart :: State,
  dfaTerminals :: S.Set State,
  dfaDelta :: M.Map State (M.Map Symbol State)
} deriving (Eq, Show)


dfaAlphabet :: DFA -> S.Set Symbol
dfaAlphabet (DFA _ _ delta) = S.fromList $ do 
  transitions <- M.elems delta
  M.keys transitions

dfaStates :: DFA -> S.Set State
dfaStates (DFA start terminals delta) = S.union deltaStates $ S.insert start terminals
  where 
    deltaStates :: S.Set State
    deltaStates = S.union statesFrom statesTo
    statesFrom = M.keysSet delta
    statesTo = S.fromList $ do 
      transitions <- M.elems delta
      M.elems transitions

prettyPrint :: DFA -> String
prettyPrint dfa@(DFA start terminals delta) = printf 
   "Alphabet: %s   \n\
    \States: %s   \n\
    \Start: %s    \n\
    \Terminals: %s\n\
    \%s" alph stts strt term delt
  where
    alph = printSymbols $ dfaAlphabet dfa
    stts = printStates $ dfaStates dfa
    strt = stName start
    term = printStates terminals
    delt = intercalate "\n" $ do
      (st, transitions) <- M.toList delta
      (st', symbols) <- M.toList $ associate (\(sym, st2) -> (st2, sym)) $ M.toList transitions
      return $ printf "%s -> %s: %s" (stName st) (stName st') (spaceSeparated $ map symName symbols)
    printStates stateSet = spaceSeparated $ map stName $ S.elems stateSet
    printSymbols symbolSet = spaceSeparated $ map symName $ S.elems symbolSet
    spaceSeparated = intercalate " " 
