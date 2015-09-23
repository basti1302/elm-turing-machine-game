module Game where

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe exposing (withDefault)
import Signal exposing (Signal, (<~), (~), foldp)
import Time exposing (every, second)

import Cell exposing (..)
import Symbol exposing (..)
import Machine exposing (..)

initialMachine : Machine
initialMachine =
  { state = initialState
  , tape = Array.fromList [ Cell blank ]
  , head = 0
  , stopped = False
  }

{-
Drops the float from the timed signal and executes one step of the Turing
machine program.
-}
tick : Float -> Machine -> Machine
tick _ machine =
  case machine.stopped of
    False -> executeStep machine
    True -> machine

{-
Triggers a tick every second
-}
mainSignal : Signal (Machine)
mainSignal = foldp tick initialMachine (every second)

{-
Convert a cell into an HTML span.
-}
renderCell : Cell -> Html
renderCell cell = span
  [ class "cell"
  , style [("background-color", (toString cell.symbol))] -- TODO convert to lower case
  ]
  [text ""]

{-
Renders the game view.
-}
main : Signal Html
main =
  (\ machine ->
    section
    [ id "game" ]
    [ h1 [] [ text "Turing Machine Puzzler" ]
    , h2 [] [ text <| toString machine.state ]
    , div [] (Array.toList (Array.map renderCell machine.tape))
    ]
  ) <~ mainSignal
