module Game.MachineView
  ( Model
  , initEmpty
  , init
  , Action ( ExecuteMachineStep
           , SwitchToProgram
           , SwitchToLevelSelect
           , GoToNextLevel)
  , update
  , view
  , viewWonLost
  ) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Game.Machine as Machine
import Game.Program as Program
import Game.Puzzle as Puzzle
import Game.Puzzles as Puzzles
import Game.RenderPhase as RenderPhase exposing (RenderPhase)
import Game.SolvedNotSolved as SolvedNotSolved exposing (SolvedNotSolved)
import Game.Tape as Tape


-- TODO Maybe the RenderPhase should be an Action instead?? Or context? It
-- feels wrong to have it in the model. Could the same be achieved with
-- evancz/elm-effects
type alias Model = (Machine.Model, RenderPhase)


type Action =
    ExecuteMachineStep
  | SwitchToProgram
  | SwitchToLevelSelect
  | GoToNextLevel


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
          Result.Ok instructionOutput  ->
            let
               nextState = instructionOutput.state
               nextSymbol = instructionOutput.symbol
               nextMove = instructionOutput.move
            in
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


view : Signal.Address Action -> Model -> Html
view address model = viewInternal address Nothing model

viewWonLost : Signal.Address Action -> SolvedNotSolved -> Model -> Html
viewWonLost address solvedNotSolved model =
  let
    wonLostText =
      case solvedNotSolved of
        SolvedNotSolved.Solved -> "☺☻ \\o/ (ʘ‿ʘ) \\o/ ☻☺"
        otherwise -> ":-( ☹☹☹☹ ಠ_ಠ ☹☹☹☹ )-:"
    btnTryAgain = button
      [ onClick address SwitchToProgram
      , class "fa fa-wrench top-button" ]
      []
    btnLevelSelect = button
      [ onClick address SwitchToLevelSelect
      , class "fa fa-sign-out top-button" ]
      []
    btnNextLevel = button
      [ onClick address GoToNextLevel
      , class "fa fa-forward" ]
      []
    buttons = case solvedNotSolved of
      SolvedNotSolved.Solved -> span [] [ btnNextLevel, btnLevelSelect]
      otherwise              -> span [] [ btnTryAgain, btnLevelSelect ]
    wonLostMessage =
    (Just <|
      div
      [ class "won-lost-message" ]
      ([ br [][]
      , text wonLostText
      , br [][]
      , br [][]
      ] ++ [buttons])
    )
  in
    viewInternal address wonLostMessage model


viewInternal : Signal.Address Action -> Maybe Html -> Model -> Html
viewInternal address wonLostBlocker (machine, renderPhase)  =
  let
    btnProgram = button
      [ onClick address SwitchToProgram
      , class "fa fa-wrench top-button" ]
      []
    btnLevelSelect = button
      [ onClick address SwitchToLevelSelect
      , class "fa fa-sign-out top-button" ]
      []
    programContent = (Machine.view renderPhase machine)
    content = case wonLostBlocker of
      Just blocker -> blocker :: programContent
      Nothing -> programContent
  in
    div
      [ class "machine-view" ]
      [ div
        [ class "top-button-bar" ]
        [ btnProgram, btnLevelSelect ],
        div
        [ class "container" ]
        content
      ]
