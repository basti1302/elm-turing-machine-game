module Machine
  ( Model
  , init
  , predictNextStep
  , Action(ExecuteStep)
  , update
  , view) where

import Array
import Debug
import Html
import Html.Attributes

import Cell exposing (..)
import Move exposing (Move)
import Program
import RenderPhase exposing (RenderPhase)
import Symbol exposing (Symbol)
import State exposing (..)
import Tape


-- TODO Currently ignored
inputSymbols : List Symbol
inputSymbols = [ Symbol.A ]


-- TODO Currently ignored
symbols : List Symbol
symbols = List.append inputSymbols [ Symbol.blank ]


initialState : State
initialState = A


acceptingStates : List State
acceptingStates = [ HALT ]


type alias Model  =
  { state : State
  , tape : Tape.Model
  , head : Int
  , program : Program.Model
  , stopped : Bool
  }


init : Program.Model -> Model
init program =
  { state = initialState
  , tape = Array.fromList [ Cell.blank ]
  , head = 0
  , program = program
  , stopped = False
  }


{-|
Predicts the next step (the outcome of executing the Turing machine program once
on the current state of the machine) without actually performing it.
-}
predictNextStep : Model -> (State, Symbol, Move)
predictNextStep machine =
  let
    maybeInstruction = Program.execute (machine.state, read machine) machine.program
  in
    case maybeInstruction of
      Just instruction ->
        ( instruction.output.state
        , instruction.output.symbol
        , instruction.output.move
        )
      -- TODO Propagating a Result (containing an error) up in switching the
      -- machine to "stopped" without abusing the HALT state would be cleaner.
      Nothing -> (State.HALT, Symbol.blank, Move.Right)


{-|
Reads the symbol at the current position of the head
-}
read : Model -> Symbol
read machine =
  let maybeSymbol = Tape.read machine.head machine.tape
  in case maybeSymbol of
    Just symbol -> symbol
    Nothing -> Symbol.blank


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
    maybeInstruction = Program.execute (machine'.state, symbol) machine.program
  in
    case maybeInstruction of
      Just instruction -> transform
        ( instruction.output.state
        , instruction.output.symbol
        , instruction.output.move
        ) machine'
      Nothing ->
        let _ = Debug.log "Incomplete Turing machine program for" (machine'.state, symbol)
        in Debug.log "Halting Turing machine at" (transform (State.HALT, Symbol.blank, Move.Right) machine')


{-|
Takes the current complete state of a Turing machine (tape content, internal
state, head position), execute one step of the Turing machine program and put
return the next complete Turing machine state (new tape content, new internal
state, new head position).
-}
transform : (State, Symbol, Move) -> Model -> Model
transform (newState, symbolToWrite, move) machine =
  let
    writeAt = machine.head
    newHead = calcHeadPosition move machine.head
  in
    { machine |
      state   <- newState
    , tape    <- Tape.update (Tape.Write writeAt (Cell.Write symbolToWrite)) machine.tape
    , head    <- newHead
    , stopped <- List.member newState acceptingStates
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


{-|
Renders the machine to HTML.
-}
view : RenderPhase -> Model -> List Html.Html
view renderPhase machine =
  let state = case renderPhase of
    RenderPhase.StartTransition (nextState, _, _) -> nextState
    otherwise -> machine.state
  in
    [ Html.div [ Html.Attributes.class "cpu" ] [
        Html.span
        [ Html.Attributes.class ("fa " ++ State.toClass state) ] []
      ]
    , Html.div [ Html.Attributes.class "head" ] []
    , Html.div
        [ Html.Attributes.class "tape-viewport" ]
        [ Tape.view renderPhase machine.head machine.tape ]
    ]
