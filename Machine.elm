module Machine where

import Array exposing (Array, append, fromList, get, length)
import Debug
import Html exposing (..)

import Cell exposing (..)
import Move exposing (..)
import Symbol exposing (..)
import Tape exposing (..)

inputSymbols : List Symbol
inputSymbols = [ Black ]

symbols : List Symbol
symbols = List.append inputSymbols [ blank ]

type State = A | B | C | HALT

initialState : State
initialState = A

acceptingStates : List State
acceptingStates = [ HALT ]

type alias Machine  =
  { state: State
  , tape: Tape
  , head: Int
  , stopped: Bool
  }

{-
This is the actual Turing Machine program.
We need to get this as user input later.

This one is the 3-state/2-symbol busy beaver.
-}
program : (State, Symbol) -> (Symbol, State, Move)
program (state, symbol) =
  case (state, symbol) of
    (A, White) -> (Black, B, Right)
    (A, Black) -> (Black, C, Left)
    (B, White) -> (Black, A, Left)
    (B, Black) -> (Black, B, Right)
    (C, White) -> (Black, B, Left)
    (C, Black) -> (Black, HALT, Right)
    _ -> Debug.crash ("Incomplete Turing machine program " ++ toString (state, symbol))

{-
Reads the symbol at the current position of the head
-}
read : Machine -> Symbol
read machine =
  let maybeSymbol = Tape.read machine.tape machine.head
  in case maybeSymbol of
    Just symbol -> symbol
    Nothing -> Debug.crash "Could not read from tape"

{-
Takes the current complete state of a Turing machine (tape content, internal
state, head position), execute one step of the Turing machine program and put
return the next complete Turing machine state (new tape content, new internal
state, new head position).
-}
transform : Machine -> (Symbol, State, Move) -> Machine
transform machine (symbolToWrite, newState, move) =
  let
    writeAt = machine.head
    newHead = calcHeadPosition machine.head move
  in
    { state = newState
    , tape = write machine.tape symbolToWrite writeAt
    , head = newHead
    , stopped = List.member newState acceptingStates
    }

{-
Executes one step, that is, it takes the current state of the Turing machine,
reads the symbol at the head, writes a symbol, sets the new state and moves the
he
-}
executeStep : Machine -> Machine
executeStep machine =
  let
    extendedTape = extendTape machine.tape machine.head
    head' = fixHeadPosition machine.head
    machine' = { machine | tape <- extendedTape, head <- head' }
    symbol = read machine'
    (symbol', state', move) = program (machine'.state, symbol)
  in
    transform machine' (symbol', state', move)

{-
Renders the machine to HTML.
-}
renderMachine : Machine -> List Html
renderMachine machine =
  [ h2 [] [ text <| toString machine.state ]
  , renderTape machine.tape machine.head
  ]
