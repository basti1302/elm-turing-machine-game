module Cell (Model, fromSymbol, blank, Action(Write), update, view) where

import Html
import Html.Attributes

import Move exposing (Move)
import RenderPhase exposing (RenderPhase)
import Symbol exposing (Symbol)

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
view : RenderPhase -> Model -> Html.Html
view renderPhase model =
  let
    (transform, transition) = case renderPhase of
      RenderPhase.StartTransition (_, _, move) -> case move of
        Move.Left -> ("translateX(60px)", "transform 350ms ease")
        Move.Right -> ("translateX(-60px)", "transform 350ms ease")
      otherwise -> ("", "")
  in
    Html.span
    [ Html.Attributes.class ("cell-" ++ Symbol.toColor model.symbol)
    , Html.Attributes.style
      [ ("background-color", Symbol.toColor model.symbol)
      , ("width", "48px")
      , ("height", "48px")
      , ("float", "left")
      , ("margin-left", "5px")
      , ("margin-right", "5px")
      , ("border", "1px solid black")
      , ("transform", transform)
      , ("transition", transition)
      ]
    ]
    [ Html.text "" ]
