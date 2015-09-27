module Tape where

import Array exposing (..)
import Debug

import Cell exposing (..)
import Move exposing (..)
import Symbol exposing (..)

type alias Tape = Array Cell

read : Tape -> Int -> Maybe Symbol
read tape position =
   let
     _ = Debug.log "read <<" (tape, position)
     maybeCell = Array.get position tape
   in
     Debug.log "read >>"
       (case maybeCell of
         Just cell -> Just cell.symbol
         Nothing -> Nothing)

{-
Extends the tape, if necessary, before reading.
-}
extendTape : Tape -> Int -> Tape
extendTape tape position =
  let
    _ = Debug.log "ext tp <<" (tape, position, length tape)
    result =
      if | position == -1 -> Array.append (fromList [cell blank]) tape
         | position >= Array.length tape -> Array.push (cell blank) tape
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
calcHeadPosition : Int -> Move -> Int
calcHeadPosition position move =
  let _ = Debug.log "calc head <<" (position, move)
  in
    case move of
      Left -> Debug.log "<|HEAD" (position - 1)
      Right -> Debug.log "HEAD|>" (position + 1)

{-
Fix the head position in case we extended the tape to the left.
-}
fixHeadPosition : Int -> Int
-- if pos = 0 and move = Left we also extend the tape left, thus we need to
-- correct the write position
fixHeadPosition position =
  let _ = Debug.log "fix head <<" position
  in Debug.log "fix head >>" (if position == -1 then 0 else position)
