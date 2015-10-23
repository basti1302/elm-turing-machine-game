module Game.Cell
  (Model
  , fromSymbol
  , blank
  , Action(Write)
  , update
  , view
  , viewMiniature)
  where

import Html exposing (..)
import Html.Attributes exposing (..)

import Game.Move as Move exposing (Move)
import Game.RenderPhase as RenderPhase exposing (RenderPhase)
import Game.Symbol as Symbol exposing (Symbol)


type alias Model = { symbol : Symbol }


fromSymbol : Symbol -> Model
fromSymbol symbol = { symbol = symbol }


blank : Model
blank = { symbol = Symbol.blank }


type Action = Write Symbol


{-|
Writes the given symbol to the cell.
-}
update : Action -> Model -> Model
update action model =
  case action of
    Write symbol -> { model | symbol <- symbol }


{-|
Convert the cell into an HTML span.
-}
view : RenderPhase -> Model -> Html
view renderPhase model =
  let
    (transform, transition) = case renderPhase of
      RenderPhase.StartTransition (_, _, move) -> case move of
        Move.Left -> ("translateX(60px)", "transform 350ms ease")
        Move.Right -> ("translateX(-60px)", "transform 350ms ease")
        Move.None -> ("none", "none")
      otherwise -> ("", "")
  in
    span
    [ class ("cell cell-" ++ Symbol.toColor model.symbol)
    , style
      [ ("transform", transform)
      , ("transition", transition)
      ]
    ]
    [ text "" ]


{-|
Convert the cell into a very small HTML span for the miniature tape view.
-}
viewMiniature : Bool -> Model -> Html
viewMiniature head model =
  let
    classes = "mini-cell cell-" ++ (Symbol.toColor model.symbol)
    classes' =
      if head && Symbol.toColor model.symbol == "red"
        then "mini-head-altcolor " ++ classes
      else if head
        then "mini-head " ++ classes
      else classes
  in
    span
    [ class classes' ]
    [ text "" ]
