module Game (main) where

import Html exposing (Html)
import Html.Attributes
import List
import Signal exposing (Signal, (<~), (~))
import Time exposing (every, second)

import Machine


{-|
Drops the float from the timed signal and executes one step of the Turing
machine program.
-}
tick : Float -> Machine.Model -> Machine.Model
tick _ machine =
  case machine.stopped of
    False -> Machine.update Machine.ExecuteStep machine
    True -> machine


{-|
Triggers a tick every second.
-}
mainSignal : Signal (Machine.Model)
mainSignal = Signal.foldp tick Machine.init (every second)


{-|
Renders the game view.
-}
main : Signal Html
main =
  (\ machine ->
    Html.section
    [ Html.Attributes.id "game" ]
    (List.append
      [ Html.h1 [] [ Html.text "Turing Machine Puzzler" ] ]
      (Machine.view machine)
    )
  ) <~ mainSignal
