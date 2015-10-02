module MachineView (Model, init, Action(SwitchToProgram), update, view) where

import Html
import Html.Events

import Machine
import RenderPhase exposing (RenderPhase)


-- TODO Maybe the RenderPhase should be an Action instead?? Or context? It
-- feels wrong to have it in the model.
type alias Model = (Machine.Model, RenderPhase)


type Action = SwitchToProgram


init : Model
init = (Machine.init, RenderPhase.Init)


{-|
Advances the Turing machine view by one renderPhase.
-}
update: Action -> Model -> Model
update action (machine, renderPhase)  =
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


view : Signal.Address Action -> Model -> List Html.Html
view address (machine, renderPhase)  =
  List.append
    (Machine.view renderPhase machine)
    [Html.button [ Html.Events.onClick address SwitchToProgram ] [ Html.text "PROGRAM" ]]
