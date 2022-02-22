module Main where

import qualified Control.Monad
import           Parser                         ( object
                                                , runParser
                                                )
import           System.Environment             ( getArgs )
import           Validation                     ( jsonIsValid )


main :: IO ()
main = do
  filename          <- head <$> getArgs
  Just (rest, json) <- runParser object <$> readFile filename
  Control.Monad.when (null rest && jsonIsValid json)
    $ writeFile (filename ++ ".out") (show json)
