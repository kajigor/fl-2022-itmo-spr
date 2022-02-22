module JSON where
import qualified Data.Map                      as M

data JSON =
  Number Int
  | Str String
  | Array [JSON]
  | Object (M.Map String JSON)
  deriving(Eq, Ord)

instance Show JSON where
  show (Number n) = show n
  show (Str    s) = "\"" ++ s ++ "\""
  show (Array a) =
    "[" ++ drop 2 (foldr (\val acc -> acc ++ ", " ++ show val) "" a) ++ "]"
  show (Object m) =
    "{"
      ++ drop
           2
           (M.foldrWithKey
             (\key val acc -> acc ++ ", \"" ++ key ++ "\": " ++ show val)
             ""
             m
           )
      ++ "}"
