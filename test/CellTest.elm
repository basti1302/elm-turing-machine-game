import ElmTest.Test exposing  (..)
import ElmTest.Assertion exposing (..)
import ElmTest.Runner.Element exposing (..)

import Symbol exposing (..)
import Cell

tests = suite "Cell"
        [ testBlank
        , testNonBlank
        , shouldUpdate
        ]

testBlank =
  test "Blank Cell" (
    assertEqual
      { symbol = Symbol.Empty }
      Cell.blank
  )

testNonBlank =
  test "Non Blank Cell" (
    assertEqual
      { symbol = Symbol.A }
      (Cell.fromSymbol Symbol.A)
  )

shouldUpdate =
  test "Update Cell" (
    assertEqual
      { symbol = Symbol.B }
      (Cell.update (Cell.Write Symbol.B) Cell.blank)
  )

main =
  runDisplay tests
