module Game.Welcome
  ( Action(Play)
  , view
  ) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Action = Play


view : Signal.Address Action -> Html
view address =
  div [ class "welcome" ]
  [   h1 []
      [ text "Alan's Machines" ]
    , button
      [ onClick address <| Play
      , class "main-menu-button"
      ]
      [ text "Select Level" ]
  ]
