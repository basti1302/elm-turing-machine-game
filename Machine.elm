module Machine where

import Array exposing (Array, append, fromList, get, length)
import Debug

import Cell exposing (..)
import Symbol exposing (..)

inputSymbols : List Symbol
inputSymbols = [ Black ]

symbols : List Symbol
symbols = List.append inputSymbols [ blank ]

type State = A | B | C | HALT

initialState : State
initialState = A

acceptingStates : List State
acceptingStates = [ HALT ]

type Move = Left | Right

type alias Tape = Array Cell

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
  let maybeCell = Array.get machine.head machine.tape
  in case maybeCell of
    Just cell -> cell.symbol
    Nothing -> Debug.crash "Could not read from tape"

{-
Extends the tape, if necessary, before moving the head.
-}
extendTape : Tape -> Int -> Move -> Tape
extendTape tape position move =
  let
    _ = Debug.log "ext tp <<" (tape, position, move, length tape)
    result =
      if | position == 0 && move == Left -> Array.append (Array.fromList [cell blank]) tape
         | position >= (Array.length tape - 1) && move == Right -> Array.push (cell blank) tape
         | otherwise -> tape
  in Debug.log "ext tp >>" result

{-
Writes a symbol to the cell at the given position, extending the tape, if
necessary.
-}
write : Tape -> Symbol -> Int -> Tape
write tape symbol position =
  let
    _ = Debug.log "write <<" (tape, symbol, position)
    result = Array.set position (cell symbol) tape
  in
    Debug.log "write >>" result

{-
Calculates the new position of the head.
-}
moveHead : Int -> Move -> Int
moveHead position move =
  let _ = Debug.log "move <<" (position, move)
  in
    case move of
      Left -> Debug.log "<|HEAD" (if position == 0 then 0 else position - 1)
      Right -> Debug.log "HEAD|>" (position + 1)

{-
Calculates the new position of the head.
-}
writePosition : Int -> Move -> Int
writePosition position move =
  let _ = Debug.log "write pos <<" (position, move)
  in Debug.log "write pos >>" (if position == 0 && move == Left then 1 else position)

{-
Takes the current complete state of a Turing machine (tape content, internal
state, head position), execute one step of the Turing machine program and put
return the next complete Turing machine state (new tape content, new internal
state, new head position).
-}
transform : Machine -> (Symbol, State, Move) -> Machine
transform machine (symbolToWrite, newState, move) =
  let
    _ = Debug.log "transf <<" (machine.tape, symbolToWrite, newState, move)
    newTape = extendTape machine.tape machine.head move
    writeAt = writePosition machine.head move
    newHead = moveHead machine.head move
  in
    Debug.log "transf >>"
    { state = newState
    , tape = write newTape symbolToWrite writeAt
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
    symbol = read machine
    (symbol', state', move) = program (machine.state, read machine)
  in
    transform machine (symbol', state', move)
