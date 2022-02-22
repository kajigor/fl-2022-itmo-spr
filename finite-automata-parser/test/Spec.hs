import qualified Data.Map                      as M
import qualified Data.Maybe
import           JSON                           ( JSON
                                                  ( Array
                                                  , Number
                                                  , Object
                                                  , Str
                                                  )
                                                )
import           Parser                         ( Parser(runParser)
                                                , object
                                                )
import           Test.HUnit
import           Validation                     ( jsonIsValid )

main :: IO Counts
main = do
  runTestTT parserTests
  runTestTT validationTests

parse :: String -> Maybe (String, JSON)
parse = runParser object

unpack :: String -> JSON
unpack = snd . Data.Maybe.fromJust . parse

parserTests :: Test
parserTests = TestList
  [ TestCase (assertEqual "" Nothing (parse ""))
  , TestCase (assertEqual "" (Just ("", Object M.empty)) (parse "{}"))
  , TestCase
    (assertEqual ""
                 (Just ("", Object (M.fromList [("key", Str "value")])))
                 (parse "{\"key\": \"value\"}")
    )
  , TestCase
    (assertEqual
      ""
      (Just ("", Object (M.fromList [("arr", Array [Number 1, Number 2])])))
      (parse "{\"arr\": [1, 2]}")
    )
  , TestCase
    (assertEqual
      ""
      (Just
        ( ""
        , Object
          (M.fromList
            [("key", Object (M.fromList [("nested", Array [Str "2"])]))]
          )
        )
      )
      (parse "{\"key\": {\"nested\": [\"2\"]}}")
    )
  ]

validationTests :: Test
validationTests = TestList
  [ TestCase (assertBool "" (not $ jsonIsValid $ unpack "{}"))
  , TestCase
    (assertBool
      ""
      ( jsonIsValid
      $ unpack
          "{\"alphabet\": [], \"states\": [1], \"terminals\": [], \"start\": 1, \"sigma\": []}"
      )
    )
  , TestCase
    (assertBool
      ""
      ( not
      $ jsonIsValid
      $ unpack
          "{\"alphabet\": [[1]], \"states\": [1], \"terminals\": [], \"start\": 1, \"sigma\": []}"
      )
    )
  , TestCase
    (assertBool
      ""
      ( not
      $ jsonIsValid
      $ unpack
          "{\"alphabet\": [], \"states\": [1], \"terminals\": [], \"start\": 1, \"sigma\": [1, 2]}"
      )
    )
  , TestCase
    (assertBool
      ""
      ( not
      $ jsonIsValid
      $ unpack
          "{\"alphabet\": [], \"states\": [], \"terminals\": [], \"start\": {}, \"sigma\": []}"
      )
    )
  , TestCase
    (assertBool
      ""
      ( not
      $ jsonIsValid
      $ unpack
          "{\"alphabet\": [], \"states\": [], \"terminals\": [], \"start\": 2, \"sigma\": []}"
      )
    )
  ]
