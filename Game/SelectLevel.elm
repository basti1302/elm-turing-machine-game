module Game.SelectLevel
  ( Model
  , init
  , Action(Select)
  , update
  , view
  ) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Game.Puzzle as Puzzle
import Game.Puzzles as Puzzles


type alias ModelItem =
  { puzzle : Puzzle.Model
  , expanded : Bool }


initItem : Puzzle.Model -> ModelItem
initItem puzzle =
  { puzzle = puzzle
  , expanded = False }

type alias Model =
  List ModelItem


init : Model
init = List.map initItem Puzzles.init


type Action = Expand ModelItem | CollapseAll | Select Puzzle.Model


updateExpanded : ModelItem -> ModelItem -> ModelItem
updateExpanded newExpandedItem item =
  if newExpandedItem == item
    then { item | expanded <- True}
    else { item | expanded <- False}


collapseAll : ModelItem -> ModelItem
collapseAll item =
  { item | expanded <- False}


update : Action -> Model -> Model
update action model =
  case action of
    Expand newExpandedItem -> List.map (updateExpanded newExpandedItem) model
    CollapseAll -> List.map collapseAll model
    otherwise -> model


tagSelect : Puzzle.Action -> Action
tagSelect action =
  case action of
    Puzzle.Select puzzle -> Select puzzle


viewPuzzle : Signal.Address Action -> ModelItem -> Html
viewPuzzle address item =
  let
    puzzle = item.puzzle
    clickHandler =
      if item.expanded
        then [ onClick address <| CollapseAll ]
        else [ onClick address <| Expand item ]
  in
    div []
    [ span
      clickHandler
      [ if item.expanded
        then Puzzle.viewDetails (Signal.forwardTo address tagSelect) puzzle
        else Puzzle.viewShort (Signal.forwardTo address tagSelect) puzzle ]
    ]


view : Signal.Address Action -> Model -> Html
view address model =
  let
    levels =
      List.map (viewPuzzle address) model
  in
    div
      [ class "levelSelection" ]
      [ div [] levels ]
