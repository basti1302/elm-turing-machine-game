module Game.Program
  ( Model
  , init
  , Action(SwitchToMachine, SwitchToLevelSelect)
  , execute
  , update
  , view
  ) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra

import Game.Instruction as Instruction
import Game.Move as Move
import Game.Puzzle as Puzzle
import Game.State as State exposing (State)
import Game.Symbol as Symbol exposing (Symbol)
import Game.Tape as Tape


-- A Dict Instruction.Input Instrution.Output
-- would be much nicer but user defined types are not comparable, nor
-- are tuples of user defined types. See
-- https://github.com/elm-lang/elm-compiler/issues/774 and
type alias Model =
  { program : List Instruction.Model
  , puzzle : Puzzle.Model
  }


type Action =
    Reset
  | SwitchToMachine
  | SwitchToLevelSelect
  | Modify Instruction.Input Instruction.Action


init : Puzzle.Model -> Model
init puzzle =
  let
    inputStates = List.filter (\ s -> s /= State.HALT ) puzzle.states
    tapeAlphabet = puzzle.tapeAlphabet
    cartesianProduct = List.Extra.lift2 (,) inputStates tapeAlphabet
    createInstruction =
      (\ (state, symbol) -> Instruction.fromInput state symbol )
    program =
      List.map createInstruction cartesianProduct
  in
    { program = program
    , puzzle = puzzle
    }


{-|
Retrieves the matching instruction from the Turing machine program for the given
input state and input symbol.
-}
execute : (State, Symbol) -> Model -> Result String Instruction.Model
execute (state, symbol) { program } =
  let
    candidates = List.filter
      (\ instruction ->
           instruction.input.state == state &&
           instruction.input.symbol == symbol)
      program
    instructionResult =
      if List.length candidates == 1
        then List.head candidates
        else Nothing
  in
    case instructionResult of
      Just instr -> Result.Ok <| handleFinalStep instr
      Nothing -> Result.Err <| "No instruction for " ++
        toString (state, symbol)


handleFinalStep : Instruction.Model -> Instruction.Model
handleFinalStep instruction =
  if instruction.output.state == State.HALT
  then
    let
      output = instruction.output
      output' = { output | move <- Move.None }
    in { instruction | output <- output'}
  else instruction


update : Action -> Model -> Model
update action model =
  case action of
    Reset -> init model.puzzle
    Modify rowId rowAction ->
      let updateRow instruction =
        if (rowId.symbol == instruction.input.symbol &&
            rowId.state == instruction.input.state)
          then Instruction.update rowAction instruction
          else instruction
      in
        { model | program <- (List.map updateRow model.program) }


convertSignal : Signal.Address Action -> Instruction.Model -> Signal.Address Instruction.Action
convertSignal address instruction =
  Signal.forwardTo address (Modify instruction.input)


viewWithInput : Signal.Address Action ->
      (Signal.Address Instruction.Action -> Instruction.Model -> Html) ->
      Instruction.Model ->
      Html
viewWithInput address renderFunction instruction =
  renderFunction (convertSignal address instruction) instruction


viewRowInputSymbols : List Instruction.Model -> Html
viewRowInputSymbols program =
  let
    tdsInputSymbols = List.map Instruction.viewInputSymbol program
    tdsInputSymbolsWithHeader =
      (td [ rowspan 2
          , class "fa fa-angle-right"
          , style [("display", "table-cell")]
          ] [])
      :: tdsInputSymbols
  in
    tr [] tdsInputSymbolsWithHeader


viewRowInputStates : List Instruction.Model -> Html
viewRowInputStates program =
  tr [] <| List.map Instruction.viewInputState program


viewRowSpacers : List Instruction.Model -> Html
viewRowSpacers program =
  tr [] <| List.repeat (List.length program) Instruction.spacer


viewRowOutputSymbols : Signal.Address Action
                       -> List Instruction.Model
                       -> Puzzle.Model
                       -> Html
viewRowOutputSymbols address program puzzle =
  let
    renderOutputSymbol = Instruction.viewOutputSymbol puzzle.tapeAlphabet
    tdsOutputSymbols =
      List.map (viewWithInput address renderOutputSymbol) program
    tdsOutputSymbolsWithHeader =
      (td [ rowspan 3
          , class "fa fa-angle-left"
          , style [("display", "table-cell")]
          ] [])
      :: tdsOutputSymbols
  in
   tr [] tdsOutputSymbolsWithHeader


viewRowOutputStates : Signal.Address Action
                      -> List Instruction.Model
                      -> Puzzle.Model
                      -> Html
viewRowOutputStates address program puzzle =
  let
    renderOutputState = Instruction.viewOutputState puzzle.states
  in
    tr [] <|
      List.map (viewWithInput address renderOutputState) program


viewRowMoves : Signal.Address Action -> List Instruction.Model -> Html
viewRowMoves address program =
  tr [] <|
    List.map (viewWithInput address Instruction.viewMove) program


topButton : Signal.Address Action -> Action -> String -> Html
topButton address action icon =
  button
    [ onClick address action
    , class <| "fa fa-" ++ icon ++ " top-button" ]
    []


view : Signal.Address Action -> Model -> Html
view address { program, puzzle } =
  let
    btnExecute = topButton address SwitchToMachine "play"
    btnReset = topButton address Reset "refresh"
    btnLevelSelect = topButton address SwitchToLevelSelect "sign-out"
    rows =
       [ viewRowInputSymbols program
       , viewRowInputStates program
       , viewRowSpacers program
       , viewRowOutputSymbols address program puzzle
       , viewRowOutputStates address program puzzle
       , viewRowMoves address program
       ]
    tableContent = [ tbody [] rows ]
    programTable = table [ class "program" ] tableContent
    puzzleInputRow =
      div [ class "mini-tape" ]
      [ span [ class "mini-tape-label fa fa-angle-double-right" ] []
      , Tape.viewMiniatureWithHead puzzle.initialHeadPosition puzzle.input
      ]
    puzzleResultRow =
      div [ class "mini-tape" ]
      [ span [ class "mini-tape-label fa fa-angle-double-left" ] []
      , Tape.viewMiniature puzzle.result
      ]
    middlePuzzleRow =
      div [ class "mini-tape-middle-row" ]
      [ span [ class "fa fa-chevron-down" ] []
      , span [ class "fa fa-chevron-down" ] []
      ]
  in
    div
      [ class "program-view" ]
      [ div
        [ class "top-button-bar" ]
        [ btnExecute, btnReset, btnLevelSelect ],
        div
        [ class "container" ]
        [ programTable
        , div
          [class "mini-tape-container-outer"]
          [ div
            [ class "mini-tape-container-inner" ]
            [ puzzleInputRow
            , middlePuzzleRow
            , puzzleResultRow
            ]
          ]
        ]
      ]
