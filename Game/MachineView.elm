module Game.MachineView
  ( Model
  , initEmpty
  , init
  , Action(ExecuteMachineStep, SwitchToProgram, SwitchToLevelSelect)
  , update
  , view
  ) where

import Html
import Html.Attributes
import Html.Events

import Game.Machine as Machine
import Game.Program as Program
import Game.Puzzle as Puzzle
import Game.Puzzles as Puzzles
import Game.RenderPhase as RenderPhase exposing (RenderPhase)
import Game.Tape as Tape


-- TODO Maybe the RenderPhase should be an Action instead?? Or context? It
-- feels wrong to have it in the model. Could the same be achieved with
-- evancz/elm-effects
type alias Model = (Machine.Model, RenderPhase)


type Action =
    ExecuteMachineStep
  | SwitchToProgram
  | SwitchToLevelSelect


initEmpty : Model
initEmpty = (Machine.init Tape.init 0 (Program.init Puzzles.default), RenderPhase.Init)


init : Puzzle.Model -> Program.Model -> Model
init puzzle program =
  (Machine.init puzzle.input puzzle.initialHeadPosition program,
   RenderPhase.Init)


maxSteps : Int
maxSteps = 25


{-|
Advances the Turing machine view by one renderPhase.
-}
update : Action -> Model -> Model
update action (machine, renderPhase)  =
  if (machine.stepCount >= maxSteps)
  then ({ machine | stopped <- True }, RenderPhase.Init)
  else case action of
    SwitchToProgram -> (machine, renderPhase)
    ExecuteMachineStep -> case machine.stopped of
      -- do nothing for a TM that is already stopped
      True  -> (machine, RenderPhase.Init)
      False ->
        let
          predicted = Machine.predictNextStep machine
        in case predicted of
          Result.Err err ->
            ({ machine | stopped <- True }, RenderPhase.Init)
          Result.Ok (nextState, nextSymbol, nextMove) ->
            case renderPhase of

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



view : Signal.Address Action -> Model -> Html.Html
view address (machine, renderPhase)  =
  let
    btnProgram = Html.button
      [ Html.Events.onClick address SwitchToProgram
      , Html.Attributes.class "fa fa-wrench top-button" ]
      []
    btnLevelSelect = Html.button
      [ Html.Events.onClick address SwitchToLevelSelect
      , Html.Attributes.class "fa fa-sign-out top-button" ]
      []
  in
    Html.div
      [ Html.Attributes.class "machine-view" ]
      [ Html.div
        [ Html.Attributes.class "top-button-bar" ]
        [ btnProgram, btnLevelSelect ],
        Html.div
        [ Html.Attributes.class "container" ]
        (Machine.view renderPhase machine)
      ]
