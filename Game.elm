module Game (main) where

import Html exposing (Html)
import Html.Attributes
import List
import Signal exposing (Signal, (<~), (~))
import Time exposing (every, millisecond)

import Move exposing (Move)
import Machine
import RenderPhase exposing (RenderPhase)
import State exposing (State)


{-|
Drops the float from the timed signal and executes one step of the Turing
machine program.
-}
tick : Float -> (Machine.Model, RenderPhase) -> (Machine.Model, RenderPhase)
tick _ (machine, renderPhase)  =
  case machine.stopped of
    False ->
      let
        (nextSymbol, nextState, nextMove) = Machine.predictNextStep machine
      in case renderPhase of

        -- Init -> WriteSymbol (write the next symbol to head's position)
        RenderPhase.Init -> (machine, RenderPhase.WriteSymbol nextSymbol)

        -- WriteSymbol -> StartTransition (start animation of tape/head
        -- according to move direction)
        RenderPhase.WriteSymbol _ ->
          (machine, RenderPhase.StartTransition
            (nextSymbol, nextState, nextMove))

        -- StartTransition -> CompleteStep (actually update the TM's state)
        RenderPhase.StartTransition _ ->
          let machine' = Machine.update Machine.ExecuteStep machine
          in (machine', RenderPhase.CompleteStep)

        -- CompleteStep -> Init (set render phase back to first state)
        RenderPhase.CompleteStep -> (machine, RenderPhase.Init)
    True -> (machine, RenderPhase.Init)


{-|
Triggers a tick every second.
-}
mainSignal : Signal (Machine.Model, RenderPhase)
mainSignal =
  Signal.foldp tick (Machine.init, RenderPhase.Init) (every (300 * millisecond))


{-|
Renders the game view.
-}
main : Signal Html
main =
  (\ (machine, renderPhase) ->
    Html.div
      [ Html.Attributes.class "game" ]
      (Machine.view renderPhase machine)
  ) <~ mainSignal
