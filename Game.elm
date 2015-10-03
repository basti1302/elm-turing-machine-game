module Game (main) where

import Html
import Html.Attributes
import List
import Signal exposing (Signal, (<~), (~))
import Time exposing (every, millisecond)

import Move exposing (Move)
import MachineView
import ProgramView
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


{-|
Initializes the model and the context.
-}
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


{-|
Generates a tick every 300 milliseconds that is used to execute a machineProgram.
-}
tickSignal : Signal Action
tickSignal =
  (\ _ -> (MachineAction MachineView.ExecuteMachineStep))
  <~ (every <| 300 * millisecond)


{-|
The main mailbox that routes the signals from all UI input elements.
-}
mainMailbox : Signal.Mailbox Action
mainMailbox = Signal.mailbox (MachineAction MachineView.ExecuteMachineStep)


{-|
Merges the constant timed tick and the signal from UI input elements.
-}
mainSignal : Signal Action
mainSignal = Signal.merge mainMailbox.signal tickSignal


{-|
The HTML output signal.
-}
main : Signal Html.Html
main =
  let
    model =
      Signal.foldp
        update
        init
        mainSignal
  in
    (view mainMailbox.address) <~ model
