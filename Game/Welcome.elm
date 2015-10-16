module Game.Welcome
  ( Action(Play)
  , view
  ) where

import Html
import Html.Attributes
import Html.Events


type Action = Play


view : Signal.Address Action -> Html.Html
view address =
  Html.div [ Html.Attributes.class "welcome" ]
  [   Html.h1 []
      [ Html.text "Alan's Machines" ]
    , Html.button
      [ Html.Events.onClick address <| Play
      , Html.Attributes.class "main-menu-button"
      ]
      [ Html.text "Select Level" ]
  ]
