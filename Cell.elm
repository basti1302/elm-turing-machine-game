module Cell (Model, fromSymbol, blank, Action(Write), update, view, viewAsHead) where

import Html
import Html.Attributes
import String exposing (toLower)

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
view : Model -> Html.Html
view model = viewInternal False model


{-|
Convert the cell into an HTML span with a slightly different visual appearance
than a normal cell.
-}
viewAsHead : Model -> Html.Html
viewAsHead model = viewInternal True model


viewInternal : Bool -> Model -> Html.Html
viewInternal asHead model =
  let
    border = if asHead then "5px solid red" else "1px solid black"
  in
    Html.span
    [ Html.Attributes.class "cell"
    , Html.Attributes.style
      [ ("background-color" , toLower <| toString model.symbol)
      , ("width", "50px")
      , ("height", "50px")
      , ("float", "left")
      , ("margin-left", "5px")
      , ("margin-right", "5px")
      , ("border", "1px solid black")
      ]
    ]
    [ Html.text "" ]
