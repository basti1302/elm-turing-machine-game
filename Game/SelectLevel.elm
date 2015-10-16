module Game.SelectLevel
  ( Model
  , init
  , Action(Select)
  , update
  , view
  ) where

import Html
import Html.Attributes
import Html.Events

import Game.Puzzle as Puzzle
import Game.Puzzles as Puzzles


type alias Model = Puzzles.Model


init : Model
init = Puzzles.init


type Action = Select Puzzle.Model


update : Action -> Model -> Model
update action model =
  model


viewPuzzle : Signal.Address Action -> Puzzle.Model -> Html.Html
viewPuzzle address puzzle =
  Html.li []
  [ Html.a
    [ Html.Events.onClick address <| Select puzzle ]
    [ Puzzle.view puzzle ]
  ]


view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    levels = List.map (viewPuzzle address) model
  in
    Html.div
      [ Html.Attributes.class "levelSelection" ]
      [ Html.ul [] levels ]
