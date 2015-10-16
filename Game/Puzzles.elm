module Game.Puzzles where

import Game.Cell as Cell
import Game.Puzzle as Puzzle
import Game.Symbol as Symbol exposing (Symbol)
import Game.Tape as Tape


type alias Model = List Puzzle.Model


-- TODO Can't set head into middle of tape :-(
-- Required for Fill In Both Directions


init : Model
init =
  [ findAndErase, fillUntil ] --, a, b, c ]


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
    Puzzle.init
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
    Puzzle.init
      "Fill Until"
      "Move to the right and make all cells red while you move, until you find the first red cell, then halt."
      input
      result


{-
a : Puzzle.Model
a = Puzzle.init
      "Lorem Ipsum"
      "Lorem Ipsum Dolor Sic Amet Consecitur Lorem Ipsum Dolor Sic Amet Consecitur Lorem Ipsum Dolor Sic Amet Consecitur."
      (Tape.fromList [])
      (Tape.fromList [])


b : Puzzle.Model
b = Puzzle.init
  "Lorem Ipsum"
  "Lorem Ipsum Dolor Sic Amet Consecitur Lorem Ipsum Dolor Sic Amet Consecitur Lorem Ipsum Dolor Sic Amet Consecitur."
  (Tape.fromList [])
  (Tape.fromList [])


c : Puzzle.Model
c = Puzzle.init
  "Lorem Ipsum"
  "Lorem Ipsum Dolor Sic Amet Consecitur Lorem Ipsum Dolor Sic Amet Consecitur Lorem Ipsum Dolor Sic Amet Consecitur."
  (Tape.fromList [])
  (Tape.fromList [])
-}
