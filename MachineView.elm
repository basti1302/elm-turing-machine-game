module MachineView
  ( Model
  , init
  , initWithProgram
  , Action(ExecuteMachineStep, SwitchToProgram)
  , update
  , view
  ) where

import Html
import Html.Attributes
import Html.Events

import Machine
import Program
import RenderPhase exposing (RenderPhase)


-- TODO Maybe the RenderPhase should be an Action instead?? Or context? It
-- feels wrong to have it in the model.
type alias Model = (Machine.Model, RenderPhase)


type Action = ExecuteMachineStep | SwitchToProgram


init : Model
init = (Machine.init Program.init, RenderPhase.Init)


initWithProgram : Program.Model -> Model
initWithProgram program = (Machine.init program, RenderPhase.Init)


{-|
Advances the Turing machine view by one renderPhase.
-}
update: Action -> Model -> Model
update action (machine, renderPhase)  =
  case action of
    SwitchToProgram -> (machine, renderPhase)
    ExecuteMachineStep -> case machine.stopped of
      False ->
        let (nextState, nextSymbol, nextMove) =
          (Machine.predictNextStep machine)
        in case renderPhase of

          -- Init -> WriteSymbol (write the next symbol to head's position)
          RenderPhase.Init -> (machine, RenderPhase.WriteSymbol nextSymbol)

          -- WriteSymbol -> StartTransition (start animation of tape/head
          -- according to move direction)
          RenderPhase.WriteSymbol _ ->
            (machine, RenderPhase.StartTransition
              (nextState, nextSymbol, nextMove))

          -- StartTransition -> CompleteStep (actually update the TM's state)
          RenderPhase.StartTransition _ ->
            let machine' = Machine.update Machine.ExecuteStep machine
            in (machine', RenderPhase.CompleteStep)

          -- CompleteStep -> Init (set render phase back to first state)
          RenderPhase.CompleteStep -> (machine, RenderPhase.Init)
      True -> (machine, RenderPhase.Init)


view : Signal.Address Action -> Model -> Html.Html
view address (machine, renderPhase)  =
  let
    btnProgram = Html.button
      [ Html.Events.onClick address SwitchToProgram
      , Html.Attributes.class "fa fa-cog" ]
      []
  in
    Html.div
      [ Html.Attributes.class "machine-view" ]
      [ Html.div
        [ Html.Attributes.class "buttons" ]
        [ btnProgram ],
        Html.div
        [ Html.Attributes.class "container" ]
        (Machine.view renderPhase machine)
      ]
