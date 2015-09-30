module Tape (Model, init, read, Action(Write, Extend), update, view) where

import Array
import Debug
import Html
import Html.Attributes

import Cell
import Move exposing (Move)
import RenderPhase exposing (RenderPhase)
import Symbol exposing (Symbol)


type alias Model = Array.Array Cell.Model


{-|
Initializes an empty tape.
-}
init : Model
init = Array.fromList []


type Action = Write Int Cell.Action | Extend Int


{-|
Updates the tape by either writing to it or expanding it.
-}
update : Action -> Model -> Model
update action model =
  case action of
    Extend position -> extend position model
    Write position cellAction -> write position cellAction model


{-|
Extends the tape, if necessary, that is, if the given position is outside the
current valid range of tape indices.
-}
extend : Int -> Model -> Model
extend position model =
  if | position == -1 -> Array.append (Array.fromList [Cell.blank]) model
     | position >= Array.length model -> Array.push (Cell.blank) model
     | otherwise -> model


{-|
Writes a symbol to the cell at the given position.
-}
write : Int -> Cell.Action -> Model -> Model
write position cellAction model =
  let
    maybeCell = (Array.get position model)
    cell' = case maybeCell of
      Just cell -> (Cell.update cellAction) cell
      Nothing -> Debug.crash "No cell to write to"
  in
    Array.set position cell' model


{-|
Reads the symbol from the tape at the given position.
-}
read : Int -> Model -> Maybe Symbol
read position model =
   let
     maybeCell = Array.get position model
   in case maybeCell of
     Just cell -> Just cell.symbol
     Nothing -> Nothing


{-|
Renders the tape to HTML.
-}
view : RenderPhase -> Int -> Model -> Html.Html
view renderPhase head tape =
  let
    show = 4
    missingLeft = max (show - head) 0 -- cells we need to append at the left edge
    deltaRight = Array.length tape - head - 1 -- # of cells between head and right edge
    missingRight = max (show - deltaRight) 0 -- cells we need to append at the right edge

    -- append cells so that we always show a certain number of cells left and right of head
    extendedTape =
      Array.append
        (Array.append (Array.repeat missingLeft Cell.blank) tape) -- prepend cells left
        (Array.repeat missingRight Cell.blank) -- append cells right
    newHead = head + missingLeft -- fix head # if we prepended left

    -- drop all cells at start/end except head and "show" cells left and right of head
    leftSide = Array.slice (newHead - show) newHead extendedTape
    cellAtHeadMaybe = Array.get newHead extendedTape
    cellAtHead =
      case renderPhase of
        RenderPhase.WriteSymbol symbol ->
          Cell.fromSymbol symbol
        RenderPhase.StartTransition (symbol, state, move) ->
          Cell.fromSymbol symbol
        otherwise ->
          case cellAtHeadMaybe of
            Just cell -> cell
            Nothing -> Debug.crash "No cell at head"
    rightSide = Array.slice (newHead + 1) (newHead + show + 1) extendedTape

    -- apply render function
    leftRendered = Array.map (Cell.view renderPhase) leftSide
    leftAndHeadRendered = Array.push (Cell.view renderPhase cellAtHead) leftRendered
    rightRendered = Array.map (Cell.view renderPhase) rightSide
  in
    Html.div
      [Html.Attributes.class "tape"]
        <| Array.toList
        <| Array.append leftAndHeadRendered rightRendered
