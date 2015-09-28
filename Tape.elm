module Tape where

import Array exposing (..)
import Debug
import Html exposing (..)

import Cell exposing (..)
import Move exposing (..)
import Symbol exposing (..)

type alias Tape = Array Cell

read : Tape -> Int -> Maybe Symbol
read tape position =
   let
     maybeCell = get position tape
   in case maybeCell of
     Just cell -> Just cell.symbol
     Nothing -> Nothing

{-
Extends the tape, if necessary, before reading.
-}
extendTape : Tape -> Int -> Tape
extendTape tape position =
  if | position == -1 -> append (fromList [cell blank]) tape
     | position >= length tape -> push (cell blank) tape
     | otherwise -> tape

{-
Writes a symbol to the cell at the given position, extending the tape, if
necessary.
-}
write : Tape -> Symbol -> Int -> Tape
write tape symbol position =
  set position (cell symbol) tape

{-
Calculates the new position of the head.
-}
calcHeadPosition : Int -> Move -> Int
calcHeadPosition position move =
  case move of
    Left -> position - 1
    Right -> position + 1

{-
Fix the head position in case we extended the tape to the left.
-}
fixHeadPosition : Int -> Int
-- If pos = -1 we just extended the tape to left. In these cases we need to fix
-- correct the head position.
fixHeadPosition position =
  if position == -1 then 0 else position

{-
Renders the tape to HTML.
-}
renderTape : Tape -> Int -> Html
renderTape tape head =
  let
    show = 4
    missingLeft = max (show - head) 0 -- cells we need to append at the left edge
    deltaRight = length tape - head - 1 -- # of cells between head and right edge
    missingRight = max (show - deltaRight) 0 -- cells we need to append at the right edge

    -- append cells so that we always show a certain number of cells left and right of head
    extendedTape =
      append
        (append (repeat missingLeft (Cell blank)) tape) -- prepend cells left
        (repeat missingRight (Cell blank)) -- append cells right
    newHead = head + missingLeft -- fix head # if we prepended left

    -- drop all cells at start/end except head and "show" cells left and right of head
    leftSide = slice (newHead - show) newHead extendedTape
    cellAtHeadMaybe = get newHead extendedTape
    cellAtHead =
      case cellAtHeadMaybe of
        Just cell -> cell
        Nothing -> Debug.crash "No cell at head"
    rightSide = slice (newHead + 1) (newHead + show + 1) extendedTape

    -- apply render function
    leftRendered = map renderCell leftSide
    leftAndHeadRendered = push (renderHead cellAtHead) leftRendered
    rightRendered = map renderCell rightSide
  in
    div [] <| toList <| append leftAndHeadRendered rightRendered
