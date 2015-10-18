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


viewPuzzle : Signal.Address Action -> ModelItem -> Html.Html
viewPuzzle address item =
  let
    puzzle = item.puzzle
    clickHandler =
      if item.expanded
        then [ Html.Events.onClick address <| CollapseAll ]
        else [ Html.Events.onClick address <| Expand item ]
  in
    Html.div []
    [ Html.span
      clickHandler
      [ if item.expanded
        then Puzzle.viewDetails (Signal.forwardTo address tagSelect) puzzle
        else Puzzle.viewShort (Signal.forwardTo address tagSelect) puzzle ]
    ]


view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    levels =
      List.map (viewPuzzle address) model
  in
    Html.div
      [ Html.Attributes.class "levelSelection" ]
      [ Html.div [] levels ]
