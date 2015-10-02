module Game (main) where

import Html
import Html.Attributes
import List
import Signal exposing (Signal, (<~), (~))
import Time exposing (every, millisecond)

import Move exposing (Move)
import MachineView
import RenderPhase exposing (RenderPhase)
import ProgramView
import StartApp.Simple as StartApp
import State exposing (State)
import Screen exposing (Screen)


type alias Model =
  { machineView : MachineView.Model
  , programView : ProgramView.Model
  }


type alias Context = { view : Screen }


type Action
  = MachineAction MachineView.Action
  | ProgramAction ProgramView.Action


init : (Model, Context)
init =
  ({ machineView = MachineView.init
   , programView = ProgramView.init
   }, { view = Screen.Program })

{-|
Updates the current view on each signal tick.
-}
update : Action -> (Model, Context) -> (Model, Context)
update action (game, context) =
  case action of
    MachineAction machineAction ->
      case machineAction of
        MachineView.SwitchToProgram ->
          (game, { context | view <- Screen.Program })
        otherwise ->
          let
            (machine', renderPhase') = MachineView.update machineAction game.machineView
          in
            ({ game | machineView <- (machine', renderPhase') }, context)
    ProgramAction programAction ->
      case programAction of
        ProgramView.SwitchToMachine ->
          (game, { context | view <- Screen.Machine })
        otherwise ->
          ({ game |
             programView <- ProgramView.update programAction game.programView },
             context
          )


{--
inputSignal : Signal Float
inputSignal = every <| 300 * millisecond
--}


{-|
Triggers a tick at a constant time interval.
-}
{--
mainSignal : Signal (Screen, MachineView.Model, ProgramView.Model)
mainSignal =
  Signal.foldp update init inputSignal
_-}


{-|
Renders the game view.
-}
view : Signal.Address Action -> (Model, Context) -> Html.Html
view address (game, context) =
  let content =
    case context.view of
      Screen.Program ->
        [ ProgramView.view (Signal.forwardTo address ProgramAction) game.programView ]
      Screen.Machine ->
        MachineView.view (Signal.forwardTo address MachineAction) game.machineView
  in
    Html.div
      [ Html.Attributes.class "game" ]
      content


main : Signal Html.Html
main =
  StartApp.start { model = init, view = view, update = update }
--main = view <~ mainSignal
