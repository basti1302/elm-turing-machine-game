module ProgramView
  ( Model
  , init
  , getProgram
  , Action(SwitchToMachine)
  , update
  , view
  ) where

import Html
import Html.Attributes
import Html.Events
import StartApp.Simple as StartApp

import Program
import ProgramRow
import State exposing (State)
import Symbol exposing (Symbol)


type alias ID = Int


type alias Model =
  { rows : List (ID, ProgramRow.Model)
  , nextId : ID
  }


type Action
  = Reset
  | SwitchToMachine
  | Modify ID ProgramRow.Action


init : Model
init =
  { rows =
    [ (0, ProgramRow.init State.A Symbol.Empty)
    , (1, ProgramRow.init State.A Symbol.A)
    , (2, ProgramRow.init State.B Symbol.Empty)
    , (3, ProgramRow.init State.B Symbol.A)
    , (4, ProgramRow.init State.C Symbol.Empty)
    , (5, ProgramRow.init State.C Symbol.A)
    ]
  , nextId = 6
  }


getProgram : Model -> Program.Model
getProgram model =
  List.map (\(_, row) -> ProgramRow.toQuintupel row) model.rows

update : Action -> Model -> Model
update action model =
  case action of
    Reset -> init
    Modify id rowAction ->
      let updateRow (rowId, programRow) =
        if (id == rowId)
          then (rowId, ProgramRow.update rowAction programRow)
          else (rowId, programRow)
      in
        { model | rows <- List.map updateRow model.rows }


viewRow : Signal.Address Action -> (ID, ProgramRow.Model) -> Html.Html
viewRow address (id, row) =
  ProgramRow.view (Signal.forwardTo address (Modify id)) row


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
      rows = List.map (viewRow address) model.rows
      tbody = Html.tbody [] rows
      tableContent = thead :: [ tbody ]
      table = Html.table [ Html.Attributes.class "program" ] tableContent
      switchView = Html.button
        [ Html.Events.onClick address SwitchToMachine
        , Html.Attributes.class "switch-view" ]
        [ Html.text "EXECUTE" ]
  in
    Html.div [] (table :: [switchView])
