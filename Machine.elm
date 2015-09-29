module Machine (Model, init, Action(ExecuteStep), update, view) where

import Array
import Debug
import Html

import Cell exposing (..)
import Move exposing (Move)
import Symbol exposing (Symbol)
import Tape


inputSymbols : List Symbol
inputSymbols = [ Symbol.Black ]


symbols : List Symbol
symbols = List.append inputSymbols [ Symbol.blank ]


type State = A | B | C | HALT


initialState : State
initialState = A


acceptingStates : List State
acceptingStates = [ HALT ]


type alias Model  =
  { state: State
  , tape: Tape.Model
  , head: Int
  , stopped: Bool
  }


init : Model
init =
  { state = initialState
  , tape = Array.fromList [ Cell.blank ]
  , head = 0
  , stopped = False
  }



{-
This is the actual Turing Machine program.
We need to get this as user input later.

This one is the 3-state/2-symbol busy beaver.
-}
program : (State, Symbol) -> (Symbol, State, Move)
program (state, symbol) =
  case (state, symbol) of
    (A, Symbol.White) -> (Symbol.Black, B, Move.Right)
    (A, Symbol.Black) -> (Symbol.Black, C, Move.Left)
    (B, Symbol.White) -> (Symbol.Black, A, Move.Left)
    (B, Symbol.Black) -> (Symbol.Black, B, Move.Right)
    (C, Symbol.White) -> (Symbol.Black, B, Move.Left)
    (C, Symbol.Black) -> (Symbol.Black, HALT, Move.Right)
    _ -> Debug.crash ("Incomplete Turing machine program " ++ toString (state, symbol))


{-
Reads the symbol at the current position of the head
-}
read : Model -> Symbol
read machine =
  let maybeSymbol = Tape.read machine.head machine.tape
  in case maybeSymbol of
    Just symbol -> symbol
    Nothing -> Debug.crash "Could not read from tape"


type Action = ExecuteStep


{-|
Updates the machine by executing a step.
-}
update : Action -> Model -> Model
update action model =
  case action of
    ExecuteStep -> executeStep model


{-|
Executes one step, that is, it takes the current state of the Turing machine,
reads the symbol at the head, writes a symbol, sets the new state and moves the
he
-}
executeStep : Model -> Model
executeStep machine =
  let
    extendedTape = Tape.update (Tape.Extend machine.head) machine.tape
    head' = fixHeadPosition machine.head
    machine' = { machine | tape <- extendedTape, head <- head' }
    symbol = read machine'
    (symbol', state', move) = program (machine'.state, symbol)
  in
    transform (symbol', state', move) machine'


{-
Takes the current complete state of a Turing machine (tape content, internal
state, head position), execute one step of the Turing machine program and put
return the next complete Turing machine state (new tape content, new internal
state, new head position).
-}
transform : (Symbol, State, Move) -> Model -> Model
transform (symbolToWrite, newState, move) machine =
  let
    writeAt = machine.head
    newHead = calcHeadPosition move machine.head
  in
    { state = newState
    , tape = Tape.update (Tape.Write writeAt (Cell.Write symbolToWrite)) machine.tape
    , head = newHead
    , stopped = List.member newState acceptingStates
    }


{-|
Calculates the new position of the head.
-}
calcHeadPosition : Move -> Int -> Int
calcHeadPosition move position =
  case move of
    Move.Left -> position - 1
    Move.Right -> position + 1


{-|
Fix the head position in case we extended the tape to the left.
-}
fixHeadPosition : Int -> Int
-- If pos = -1 we just extended the tape to left. In these cases we need to fix
-- correct the head position.
fixHeadPosition position =
  if position == -1 then 0 else position


{-
Renders the machine to HTML.
-}
view : Model -> List Html.Html
view machine =
  [ Html.h2 [] [ Html.text <| toString machine.state ]
  , Tape.view machine.head machine.tape
  ]
