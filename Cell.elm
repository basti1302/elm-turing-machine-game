module Cell where

import Html exposing (..)
import Html.Attributes exposing (..)
import String exposing (toLower)

import Symbol exposing (..)

type alias Cell = { symbol : Symbol }

cell : Symbol -> Cell
cell symbol = { symbol = symbol }

blankCell: Cell
blankCell = { symbol = blank }

{-
Convert a cell into an HTML span.
-}
renderCell : Cell -> Html
renderCell cell = span
  [ class "cell"
  , style
    [ ("background-color" , toLower <| toString cell.symbol)
    , ("width", "50px")
    , ("height", "50px")
    , ("float", "left")
    , ("margin-left", "5px")
    , ("margin-right", "5px")
    , ("border", "1px solid black")
    ]
  ]
  [text ""]

renderHead : Cell -> Html
renderHead cell = span
  [ class "cell"
  , style
    [ ("background-color" , toLower <| toString cell.symbol)
    , ("width", "50px")
    , ("height", "50px")
    , ("float", "left")
    , ("margin-left", "5px")
    , ("margin-right", "5px")
    , ("border", "5px solid red")
    ]
  ]
  [text ""]
