module Game.Puzzle
  ( Model
  , init
  , isSolved
  , view
  )
  where

import Html
import Html.Attributes

import Game.Tape as Tape

type alias Model =
  { title : String
  , description : String
  , input : Tape.Model
  , result : Tape.Model
  -- , Tape Alphabet?
  -- , Set of possible states (as subset from Game.State)?
  }


init : String -> String -> Tape.Model -> Tape.Model -> Model
init title description input result =
  { title = title
  , description = description
  , input = input
  , result = result
  }


isSolved : Tape.Model -> Model -> Bool
isSolved tape puzzle =
  Tape.trim tape == Tape.trim puzzle.result


view : Model -> Html.Html
view puzzle =
  Html.div []
  [ Html.span [ Html.Attributes.class "title"] [ Html.text puzzle.title ]
  , Html.p [ Html.Attributes.class "description" ] [ Html.text puzzle.description ]
  ]
