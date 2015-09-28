module Game where

import Array exposing (fromList)
import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (append)
import Maybe exposing (withDefault)
import Signal exposing (Signal, (<~), (~), foldp)
import Time exposing (every, second)

import Cell exposing (..)
import Symbol exposing (..)
import Machine exposing (..)

initialMachine : Machine
initialMachine =
  { state = initialState
  , tape = fromList [ Cell blank ]
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
Renders the game view.
-}
main : Signal Html
main =
  (\ machine ->
    section
    [ id "game" ]
    (append
      [ h1 [] [ text "Turing Machine Puzzler" ] ]
      (renderMachine machine)
    )
  ) <~ mainSignal
