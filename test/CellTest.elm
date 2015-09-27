import ElmTest.Test exposing  (..)
import ElmTest.Assertion exposing (..)
import ElmTest.Runner.Element exposing (..)

import Symbol exposing (..)
import Cell exposing (..)

tests = suite "Cell"
        [ testBlank
        , testNonBlank
        ]

testBlank =
  test "Blank Cell" (
    assertEqual
      { symbol = White }
      blankCell
  )

testNonBlank =
  test "Non Blank Cell" (
    assertEqual
      { symbol = Black }
      (cell Black)
  )

main =
  runDisplay tests
