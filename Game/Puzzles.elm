module Game.Puzzles where

import Game.Cell as Cell
import Game.Puzzle as Puzzle
import Game.State as State exposing (State)
import Game.Symbol as Symbol exposing (Symbol)
import Game.Tape as Tape


type alias Model = List Puzzle.Model


-- TODO Can't set head into middle of tape :-(
-- Required for Fill In Both Directions


init : Model
init =
  [ findAndErase, fillUntil, appendTwo ]


default : Puzzle.Model
default =
  Puzzle.initSimple "" "" (Tape.fromList []) (Tape.fromList [])


findAndErase : Puzzle.Model
findAndErase =
  let
    input = Tape.fromList
      [ (Cell.fromSymbol Symbol.Empty)
      , (Cell.fromSymbol Symbol.Empty)
      , (Cell.fromSymbol Symbol.Empty)
      , (Cell.fromSymbol Symbol.A)
      ]
    result = Tape.fromList []
  in
    Puzzle.initSimple
      "Find and Erase"
      "Move to the right until you find the red cell, then erase it and halt."
      input
      result


fillUntil : Puzzle.Model
fillUntil =
  let
    input = Tape.fromList
      [ (Cell.fromSymbol Symbol.Empty)
      , (Cell.fromSymbol Symbol.Empty)
      , (Cell.fromSymbol Symbol.Empty)
      , (Cell.fromSymbol Symbol.Empty)
      , (Cell.fromSymbol Symbol.A)
      ]
    result = Tape.fromList
      [ (Cell.fromSymbol Symbol.A)
      , (Cell.fromSymbol Symbol.A)
      , (Cell.fromSymbol Symbol.A)
      , (Cell.fromSymbol Symbol.A)
      , (Cell.fromSymbol Symbol.A)
      ]
  in
    Puzzle.initSimple
      "Fill Until"
      "Move to the right and make all cells red while you move, until you find the first red cell, then halt."
      input
      result


appendTwo : Puzzle.Model
appendTwo =
  let
    input = Tape.fromList
      [ (Cell.fromSymbol Symbol.A)
      , (Cell.fromSymbol Symbol.A)
      , (Cell.fromSymbol Symbol.A)
      ]
    result = Tape.fromList
      [ (Cell.fromSymbol Symbol.A)
      , (Cell.fromSymbol Symbol.A)
      , (Cell.fromSymbol Symbol.A)
      , (Cell.fromSymbol Symbol.A)
      , (Cell.fromSymbol Symbol.A)
      ]
  in
    Puzzle.init
      "Append Two"
      "Given a sequence of red cells, append two red cells after the end of the sequence (that is, make the string of red cells two cells longer).
"
      [ Symbol.A ]
      [ State.A, State.B ]
      input
      result
