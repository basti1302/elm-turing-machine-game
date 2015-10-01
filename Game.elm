module Game (main) where

import Html exposing (Html)
import Html.Attributes
import List
import Signal exposing (Signal, (<~), (~))
import Time exposing (every, millisecond)

import Move exposing (Move)
import MachineView
import RenderPhase exposing (RenderPhase)
import ProgramView
import State exposing (State)
import View exposing (View)

type alias Model = (View, MachineView.Model, ProgramView.Model)


init : Model
init =
  (View.VMachine, MachineView.init, ProgramView.init)


{-|
Updates the current view on each signal tick.
-}
update : Float ->
         (View, MachineView.Model, ProgramView.Model) ->
         (View, MachineView.Model, ProgramView.Model)
update _ (view, (machine, renderPhase), programViewContent) =
  case view of
    View.VMachine ->
      let (machine', renderPhase') = MachineView.update (machine, renderPhase)
      in (View.VMachine, (machine', renderPhase'), programViewContent)
    View.VProgram ->
      (View.VProgram, (machine, renderPhase), ProgramView.update programViewContent)


{-|
Triggers a tick at a constant time interval.
-}
mainSignal : Signal (View, MachineView.Model, ProgramView.Model)
mainSignal =
  Signal.foldp update init (every (300 * millisecond))


{-|
Renders the game view.
-}
main : Signal Html
main =
  (\ (view, (machine, renderPhase), programViewContent) ->
    let content =
      case view of
        View.VProgram ->
          ProgramView.view programViewContent
        View.VMachine ->
          MachineView.view (machine, renderPhase)
    in
      Html.div
        [ Html.Attributes.class "game" ]
        content
  ) <~ mainSignal
