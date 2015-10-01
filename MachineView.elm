module MachineView (Model, init, update, view) where

import Html exposing (Html)

import Machine
import RenderPhase exposing (RenderPhase)


type alias Model = (Machine.Model, RenderPhase)


init : Model
init = (Machine.init, RenderPhase.Init)


{-|
Advances the Turing machine view by one renderPhase.
-}
update: Model -> Model
update (machine, renderPhase)  =
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


view : Model -> List Html.Html
view (machine, renderPhase)  =
  Machine.view renderPhase machine
