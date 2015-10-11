module Game.Puzzles where

import Game.Cell as Cell
import Game.Puzzle as Puzzle
import Game.Symbol as Symbol exposing (Symbol)
import Game.Tape as Tape


one : Puzzle.Model
one =
  let
    input = Tape.fromList
      [ (Cell.fromSymbol Symbol.A)
      , (Cell.fromSymbol Symbol.A)
      , (Cell.fromSymbol Symbol.A)
      ]
    result = Tape.fromList []
  in
    Puzzle.init input result


two : Puzzle.Model
two =
  let
    input = Tape.fromList []
    result = Tape.fromList
      [ (Cell.fromSymbol Symbol.A)
      , (Cell.fromSymbol Symbol.A)
      ]
  in
    Puzzle.init input result
