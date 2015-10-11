module Tests.PuzzleTest (tests, main) where

import ElmTest.Test exposing  (..)
import ElmTest.Assertion exposing (..)
import ElmTest.Runner.Element exposing (..)

import Game.Cell as Cell
import Game.Puzzle as Puzzle
import Game.Symbol as Symbol
import Game.Tape as Tape

tests = suite "Puzzle"
        [ testIsSolved ]

testIsSolved =
  test "is solved" (
    assert
      (Puzzle.isSolved
        (Tape.fromList
          [ Cell.blank
          , Cell.fromSymbol Symbol.B
          , Cell.fromSymbol Symbol.A
          , Cell.blank
          , Cell.fromSymbol Symbol.B
          , Cell.blank
          , Cell.blank
          ])
        (Puzzle.init Tape.init (Tape.fromList
          [ Cell.blank
          , Cell.blank
          , Cell.fromSymbol Symbol.B
          , Cell.fromSymbol Symbol.A
          , Cell.blank
          , Cell.fromSymbol Symbol.B
          , Cell.blank
          , Cell.blank
          , Cell.blank
          ])
        )
      )
  )

main =
  runDisplay tests
