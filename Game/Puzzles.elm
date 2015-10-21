module Game.Puzzles where

import List.Extra

import Game.Cell as Cell
import Game.Puzzle as Puzzle
import Game.State as State exposing (State)
import Game.Symbol as Symbol exposing (Symbol)
import Game.Tape as Tape


type alias Model = List Puzzle.Model


init : Model
init =
  [ justStop
  , findAndErase
  , fillUntil
  , appendOne
  , appendTwo
  , fillInBothDirections
  , fillInBothDirectionsTwoStates
  ]


nextLevel : Puzzle.Model -> Model -> Maybe Puzzle.Model
nextLevel current puzzles =
  let
    currentAndRest = List.Extra.dropWhile (\ p -> p /= current) puzzles
    rest = List.tail currentAndRest
  in
    case rest of
      Just l -> List.head l
      Nothing -> Nothing


default : Puzzle.Model
default =
  Puzzle.initSimple "" "" (Tape.fromList []) (Tape.fromList [])


justStop : Puzzle.Model
justStop =
  let
    input = Tape.fromList [ (Cell.fromSymbol Symbol.Empty) ]
    result = Tape.fromList [ (Cell.fromSymbol Symbol.Empty) ]
  in
    Puzzle.init
      "Just Stop"
      "Just stop the machine. Switch the machine to the halting state."
      [] [State.A] 0 input result


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
      "Move to the right until you find the red cell, then turn it off and halt."
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
      "Move to the right and light all cells red while you move, until you find the first red cell, then halt."
      input
      result


appendOne : Puzzle.Model
appendOne =
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
      ]
  in
    Puzzle.initSimple
      "Append One"
      "Given a sequence of red cells, append one more red cell after the end of the sequence (that is, make the string of red cells one cell longer).
"
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
      0
      input
      result


fillInBothDirections : Puzzle.Model
fillInBothDirections =
  let
    input = Tape.fromList
      [ (Cell.fromSymbol Symbol.A)
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
      "Fill in Both Directions"
      "You will start in the middle between a red cell to the left and a red cell to the right. Fill the space between those cells completely with red cells. Don't modify cells outside this area."
      [ Symbol.A ]
      [ State.A, State.B, State.C ]
      2
      input
      result


fillInBothDirectionsTwoStates : Puzzle.Model
fillInBothDirectionsTwoStates =
  let
    input = Tape.fromList
      [ (Cell.fromSymbol Symbol.A)
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
      "Fill in Both Directions II"
      "Same task as before, but this time you only have two different states at your disposal. As in \"Fill In Both Directions\", you will start in the middle between a red cell to the left and a red cell to the right. Fill the space between those cells completely with red cells. Don't modify cells outside this area."
      [ Symbol.A ]
      [ State.A, State.B ]
      2
      input
      result


