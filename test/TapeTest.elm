import ElmTest.Test exposing  (..)
import ElmTest.Assertion exposing (..)
import ElmTest.Runner.Element exposing (..)

import Array exposing (..)

import Symbol exposing (..)
import Cell exposing (..)
import Tape

tests = suite "Tape"
        [ testExtendNotNecessaryLeft
        , testExtendNotNecessaryMiddle
        , testExtendNotNecessaryRight
        , testExtendLeft
        , testExtendRight
        , testRead
        , testReadFail
        , testWrite
        -- , testWriteFail
        ]

testExtendNotNecessaryLeft =
  test "extend not necessary on left edge" (
    assertEqual
      initialTape
      (Tape.update (Tape.Extend 0) initialTape)
  )

testExtendNotNecessaryMiddle =
  test "extend not necessary in the middle" (
    assertEqual
      initialTape
      (Tape.update (Tape.Extend 1) initialTape)
  )

testExtendNotNecessaryRight =
  test "extend not necessary on right edge" (
    assertEqual
      initialTape
      (Tape.update (Tape.Extend 2) initialTape)
  )

testExtendLeft =
  test "extend left" (
    assertEqual
      (Array.fromList
        [ Cell.blank
        , Cell.fromSymbol Symbol.A
        , Cell.fromSymbol Symbol.B
        , Cell.fromSymbol Symbol.C
        ])
      (Tape.update (Tape.Extend -1) initialTape)
  )

testExtendRight =
  test "extend right" (
    assertEqual
      (Array.fromList
        [ Cell.fromSymbol Symbol.A
        , Cell.fromSymbol Symbol.B
        , Cell.fromSymbol Symbol.C
        , Cell.blank
        ])
      (Tape.update (Tape.Extend 3) initialTape)
  )

testRead =
  test "read" (
    assertEqual
      (Just Symbol.B)
      (Tape.read 1 initialTape)
  )

testReadFail =
  test "read fail" (
    assertEqual
      Nothing
      (Tape.read 3 initialTape)
  )

testWrite =
  test "write" (
    assertEqual
      (Array.fromList
        [ Cell.fromSymbol Symbol.A
        , Cell.fromSymbol Symbol.B
        , Cell.fromSymbol Symbol.A
        ])
      (Tape.update (Tape.Write 2 (Cell.Write Symbol.A)) initialTape )
  )

{-
testWriteFail =
  test "write fail" (
    assertEqual
      (Array.fromList
        [ Cell.fromSymbol Symbol.A
        , Cell.fromSymbol Symbol.B
        , Cell.fromSymbol Symbol.A
        ])
      (Tape.update (Tape.Write 3 (Cell.Write Symbol.C)) initialTape)
  )
-}

initialTape : Tape.Model
initialTape = Array.fromList
  [ Cell.fromSymbol Symbol.A
  , Cell.fromSymbol Symbol.B
  , Cell.fromSymbol Symbol.C
  ]

main =
  runDisplay tests
