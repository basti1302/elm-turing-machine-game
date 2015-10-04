module Program
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
import StartApp.Simple as StartApp

-- import Program
import Instruction
import State exposing (State)
import Symbol exposing (Symbol)


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
  , Instruction.fromInput State.A Symbol.A
  , Instruction.fromInput State.B Symbol.Empty
  , Instruction.fromInput State.B Symbol.A
  , Instruction.fromInput State.C Symbol.Empty
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


viewRow : Signal.Address Action -> Instruction.Model -> Html.Html
viewRow address instruction =
  Instruction.view (Signal.forwardTo address (Modify instruction.input)) instruction


-- TODO Maybe pivot the table and show current rows as columns? Might be better
-- in line with the Turing machine view, which is currently rather landscape-ish
-- instead of portrait-ish.
view : Signal.Address Action -> Model -> Html.Html
view address model =
  let thead = Html.thead []
        [ Html.tr []
          [ Html.td [ Html.Attributes.colspan 2 ] [ Html.text "In" ]
          , Html.td [ Html.Attributes.class "spacer" ] []
          , Html.td [ Html.Attributes.colspan 3 ] [ Html.text "Out" ]
          ]
        ]
      rows = List.map (viewRow address) model
      tbody = Html.tbody [] rows
      tableContent = thead :: [ tbody ]
      table = Html.table [ Html.Attributes.class "program" ] tableContent
      switchView = Html.button
        [ Html.Events.onClick address SwitchToMachine
        , Html.Attributes.class "switch-view" ]
        [ Html.text "EXECUTE" ]
  in
    Html.div [] (table :: [switchView])
