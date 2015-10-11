module Game.Program
  ( Model
  , init
  , Action(SwitchToMachine)
  , execute
  , update
  , view
  ) where

import Html
import Html.Attributes
import Html.Events

import Game.Instruction as Instruction
import Game.State as State exposing (State)
import Game.Symbol as Symbol exposing (Symbol)


-- A Dict Instruction.Input Instrution.Output
-- would be much nicer but user defined types are not comparable, nor
-- are tuples of user defined types. See
-- https://github.com/elm-lang/elm-compiler/issues/774 and
type alias Model = List Instruction.Model


type Action
  = Reset
  | SwitchToMachine
  | Modify Instruction.Input Instruction.Action


init : Model
init =
  [ Instruction.fromInput State.A Symbol.Empty
  , Instruction.fromInput State.B Symbol.Empty
  , Instruction.fromInput State.C Symbol.Empty
  , Instruction.fromInput State.A Symbol.A
  , Instruction.fromInput State.B Symbol.A
  , Instruction.fromInput State.C Symbol.A
  ]


{-|
Retrieves the matching instruction from the Turing machine program for the given
input state and input symbol.
-}
-- TODO Replace Maybe type be Result
execute : (State, Symbol) -> Model -> Maybe Instruction.Model
execute (state, symbol) program =
  let
    candidates = List.filter
      (\ instruction ->
           instruction.input.state == state &&
           instruction.input.symbol == symbol)
      program
  in
    if List.length candidates == 1
      then List.head candidates
      else Nothing


update : Action -> Model -> Model
update action model =
  case action of
    Reset -> init
    Modify rowId rowAction ->
      let updateRow instruction =
        if (rowId.symbol == instruction.input.symbol &&
            rowId.state == instruction.input.state)
          then Instruction.update rowAction instruction
          else instruction
      in
        List.map updateRow model


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
view address model =
  let
    tdsInputSymbols = List.map Instruction.viewInputSymbol model
    tdsInputSymbolsWithHeader =
      (Html.td [ Html.Attributes.rowspan 2 ] [ Html.text "In" ])
      :: tdsInputSymbols
    rowInputSymbols = Html.tr [] tdsInputSymbolsWithHeader
    rowInputStates = Html.tr [] <| List.map Instruction.viewInputState model

    rowSpacers = Html.tr [] <| List.repeat (List.length model) Instruction.spacer

    tdsOutputSymbols =
      List.map (viewWithInput address Instruction.viewOutputSymbol) model
    tdsOutputSymbolsWithHeader =
      (Html.td [ Html.Attributes.rowspan 3 ] [ Html.text "Out" ])
      :: tdsOutputSymbols
    rowOutputSymbols = Html.tr [] tdsOutputSymbolsWithHeader
    rowOutputStates = Html.tr [] <|
      List.map (viewWithInput address Instruction.viewOutputState) model
    rowMoves = Html.tr [] <|
      List.map (viewWithInput address Instruction.viewMove) model
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
      , Html.Attributes.class "fa fa-play" ]
      []
    btnReset = Html.button
      [ Html.Events.onClick address Reset
      , Html.Attributes.class "fa fa-refresh" ]
      []

  in
    Html.div
      [ Html.Attributes.class "program-view" ]
      [ Html.div
          [ Html.Attributes.class "buttons" ]
          [btnExecute, btnReset],
        Html.div
        [ Html.Attributes.class "container" ]
        [ table ]
      ]
