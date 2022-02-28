module Util
  ( orElse
  , associate
  ) where

import Data.List ( groupBy )
import Data.Function ( on )
import qualified Data.Map as M

orElse :: Maybe a -> a -> a
orElse mb backup = maybe backup id mb
  
associate :: Ord k => (a -> (k, v)) -> [a] -> M.Map k [v]
associate f l = M.fromList $ map (\xs -> (fst (head xs), map snd xs)) $ groupBy ((==) `on` fst) $ map f l

