module Game.Program
  ( Model
  , init
  , Action(SwitchToMachine, SwitchToLevelSelect)
  , execute
  , update
  , view
  ) where

import Html
import Html.Attributes
import Html.Events
import List.Extra

import Game.Instruction as Instruction
import Game.Puzzle as Puzzle
import Game.State as State exposing (State)
import Game.Symbol as Symbol exposing (Symbol)


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
      Just instr -> Result.Ok instr
      Nothing -> Result.Err <| "No instruction for " ++
        toString (state, symbol)


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
      (Signal.Address Instruction.Action -> Instruction.Model -> Html.Html) ->
      Instruction.Model ->
      Html.Html
viewWithInput address renderFunction instruction =
  renderFunction (convertSignal address instruction) instruction


view : Signal.Address Action -> Model -> Html.Html
view address { program, puzzle } =
  let
    tdsInputSymbols = List.map Instruction.viewInputSymbol program
    tdsInputSymbolsWithHeader =
      (Html.td [ Html.Attributes.rowspan 2 ] [ Html.text "In" ])
      :: tdsInputSymbols
    rowInputSymbols = Html.tr [] tdsInputSymbolsWithHeader
    rowInputStates = Html.tr [] <| List.map Instruction.viewInputState program

    rowSpacers = Html.tr [] <| List.repeat (List.length program) Instruction.spacer

    renderOutputSymbol = Instruction.viewOutputSymbol puzzle.tapeAlphabet
    tdsOutputSymbols =
      List.map (viewWithInput address renderOutputSymbol) program
    tdsOutputSymbolsWithHeader =
      (Html.td [ Html.Attributes.rowspan 3 ] [ Html.text "Out" ])
      :: tdsOutputSymbols
    rowOutputSymbols = Html.tr [] tdsOutputSymbolsWithHeader
    renderOutputState = Instruction.viewOutputState puzzle.states
    rowOutputStates = Html.tr [] <|
      List.map (viewWithInput address renderOutputState) program
    rowMoves = Html.tr [] <|
      List.map (viewWithInput address Instruction.viewMove) program
    rows =
       [ rowInputSymbols
       , rowInputStates
       , rowSpacers
       , rowOutputSymbols
       , rowOutputStates
       , rowMoves
       ]

    tableContent = [ Html.tbody [] rows ]
    table = Html.table [ Html.Attributes.class "program" ] tableContent
    btnExecute = Html.button
      [ Html.Events.onClick address SwitchToMachine
      , Html.Attributes.class "fa fa-play top-button" ]
      []
    btnReset = Html.button
      [ Html.Events.onClick address Reset
      , Html.Attributes.class "fa fa-refresh top-button" ]
      []
    btnLevelSelect = Html.button
      [ Html.Events.onClick address SwitchToLevelSelect
      , Html.Attributes.class "fa fa-sign-out top-button" ]
      []

  in
    Html.div
      [ Html.Attributes.class "program-view" ]
      [ Html.div
          [ Html.Attributes.class "top-button-bar" ]
          [ btnExecute, btnReset, btnLevelSelect ],
        Html.div
        [ Html.Attributes.class "container" ]
        [ table ]
      ]
