module Validation
  ( jsonIsValid
  ) where

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           JSON                           ( JSON
                                                  ( Array
                                                  , Number
                                                  , Object
                                                  , Str
                                                  )
                                                )

isNumberOrString :: JSON -> Bool
isNumberOrString (Number _) = True
isNumberOrString (Str    _) = True
isNumberOrString _          = False

subset :: Eq a => [a] -> [a] -> Bool
subset l1 l2 = all (`elem` l2) l1

fieldsPresented :: [String] -> M.Map String JSON -> Bool
fieldsPresented lst m = all (`M.member` m) lst

alphabetValid :: JSON -> Bool
alphabetValid (Array lst) = all isNumberOrString lst
alphabetValid _           = False

statesIsValid :: JSON -> Bool
statesIsValid (Array lst) = all isNumberOrString lst
statesIsValid _           = False

terminalsValid :: JSON -> JSON -> Bool
terminalsValid (Array states) (Array terminals) =
  all isNumberOrString terminals && terminals `subset` states
terminalsValid _ _ = False

startIsValid :: JSON -> JSON -> Bool
startIsValid (Array states) start =
  isNumberOrString start && start `elem` states
startIsValid _ _ = False

sigmaIsValid :: JSON -> JSON -> JSON -> Bool
sigmaIsValid (Array states) (Array alphabet) (Array ((Object m) : t)) =
  fieldsPresented ["from", "by", "to"] m
    &&     isNumberOrString (m M.! "from")
    &&     (m M.! "from")
    `elem` states
    &&     isNumberOrString (m M.! "to")
    &&     (m M.! "to")
    `elem` states
    &&     isNumberOrString (m M.! "by")
    &&     (m M.! "by")
    `elem` alphabet
    &&     sigmaIsValid (Array states) (Array alphabet) (Array t)
sigmaIsValid _ _ (Array []) = True
sigmaIsValid _ _ _          = False

jsonIsValid :: JSON -> Bool
jsonIsValid (Object m) =
  fieldsPresented ["alphabet", "states", "terminals", "start", "sigma"] m
    && alphabetValid (m M.! "alphabet")
    && statesIsValid (m M.! "states")
    && terminalsValid (m M.! "states") (m M.! "terminals")
    && startIsValid (m M.! "states") (m M.! "start")
    && sigmaIsValid (m M.! "states") (m M.! "alphabet") (m M.! "sigma")
jsonIsValid _ = False
