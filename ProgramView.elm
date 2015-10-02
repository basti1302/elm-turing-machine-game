module ProgramView
  ( Model
  , init
  , Action(SwitchToMachine)
  , update
  , view
  ) where

import Html
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp

import ProgramRow
import State exposing (..)
import Symbol exposing (..)

type alias Model =
  { topRow : ProgramRow.Model
  , bottomRow : ProgramRow.Model
  }


type Action
  = Reset
  | SwitchToMachine
  | Top ProgramRow.Action
  | Bottom ProgramRow.Action


init : Model
init =
  { topRow = ProgramRow.init A Symbol.White
  , bottomRow = ProgramRow.init A Symbol.Black
  }


update : Action -> Model -> Model
update action model =
  case action of
    Reset -> init

    Top act ->
      { model |
          topRow <- ProgramRow.update act model.topRow
      }

    Bottom act ->
      { model |
          bottomRow <- ProgramRow.update act model.bottomRow
      }


view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div []
    [ ProgramRow.view (Signal.forwardTo address Top) model.topRow
    , ProgramRow.view (Signal.forwardTo address Bottom) model.bottomRow
    , Html.button [ Html.Events.onClick address SwitchToMachine ] [ Html.text "-> MACHINE" ]
    ]

{-
main : Signal Html.Html
main =
  StartApp.start { model = init, view = view, update = update }
-}
