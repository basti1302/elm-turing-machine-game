import ElmTest.Test exposing  (..)
import ElmTest.Assertion exposing (..)
import ElmTest.Runner.Element exposing (..)

import Array exposing (..)

import Symbol exposing (..)
import Cell exposing (..)
import Tape exposing (..)

tests = suite "Tape"
        [ testExtendNotNecessaryLeft
        , testExtendNotNecessaryMiddle
        , testExtendNotNecessaryRight
        , testExtendLeft
        , testExtendRight
        , testRead
        , testReadFail
        , testWrite
        , testWriteFail
        ]

testExtendNotNecessaryLeft =
  test "extend not necessary on left edge" (
    assertEqual
      initialTape
      (extendTape initialTape 0)
  )

testExtendNotNecessaryMiddle =
  test "extend not necessary in the middle" (
    assertEqual
      initialTape
      (extendTape initialTape 1)
  )

testExtendNotNecessaryRight =
  test "extend not necessary on right edge" (
    assertEqual
      initialTape
      (extendTape initialTape 2)
  )

testExtendLeft =
  test "extend left" (
    assertEqual
      (Array.fromList [ Cell blank, Cell White, Cell Black, Cell White ])
      (extendTape initialTape -1)
  )

testExtendRight =
  test "extend right" (
    assertEqual
      (Array.fromList [ Cell White, Cell Black, Cell White, Cell blank ])
      (extendTape initialTape 3)
  )

testRead =
  test "read" (
    assertEqual
      (Just Black)
      (read initialTape 1)
  )

testReadFail =
  test "read fail" (
    assertEqual
      Nothing
      (read initialTape 3)
  )

testWrite =
  test "write" (
    assertEqual
      (Array.fromList [ Cell White, Cell Black, Cell Black ])
      (write initialTape Black 2)
  )

testWriteFail =
  test "write fail" (
    assertEqual
      (Array.fromList [ Cell White, Cell Black, Cell White ])
      (write initialTape Black 3)
  )

initialTape : Tape
initialTape = Array.fromList [ Cell White, Cell Black, Cell White ]

main =
  runDisplay tests
