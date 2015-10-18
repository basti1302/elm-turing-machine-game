module Game.Puzzle
  ( Model
  , init
  , initSimple
  , Action(Select)
  , isSolved
  , viewShort
  , viewDetails
  )
  where

import Html
import Html.Attributes
import Html.Events

import Game.Symbol as Symbol exposing (Symbol)
import Game.State as State exposing (State)
import Game.Tape as Tape


type alias Model =
  { title : String
  , description : String
  , tapeAlphabet : List Symbol -- tape symbols, excluding blank
  , states : List State
  , input : Tape.Model
  , result : Tape.Model
  }


type Action = Select Model

{-|
Initializes a puzzle with only one input symbol (red) and two states (A and HALT).
-}
initSimple :
  String ->
  String ->
  Tape.Model ->
  Tape.Model ->
  Model
initSimple title description input result =
  { title = title
  , description = description
  , tapeAlphabet = [ Symbol.Empty, Symbol.A ]
  , states = [ State.A, State.HALT ]
  , input = input
  , result = result
  }


{-|
Initializes a puzzle.

The blank symbol is automatically added to the tape alphabet, do not include it
in the inputAlphabet. The HALT state is also added to the set of states.
-}
init :
  String ->
  String ->
  List Symbol ->
  List State ->
  Tape.Model ->
  Tape.Model ->
  Model
init title description inputAlphabet states input result =
  { title = title
  , description = description
  , tapeAlphabet = Symbol.Empty :: inputAlphabet
  , states = states ++ [ State.HALT ]
  , input = input
  , result = result
  }


isSolved : Tape.Model -> Model -> Bool
isSolved tape puzzle =
  Tape.trim tape == Tape.trim puzzle.result


viewShort : Signal.Address Action -> Model -> Html.Html
viewShort address puzzle =
  Html.div []
  [ Html.span
    [ Html.Attributes.class "title"]
    [ buttonPlay address puzzle
    , Html.text puzzle.title
    ]
  ]


viewDetails : Signal.Address Action -> Model -> Html.Html
viewDetails address puzzle =
  Html.div []
  [ Html.span
    [ Html.Attributes.class "title"]
    [ buttonPlay address puzzle
    , Html.text puzzle.title
    ]
  , Html.p
    [ Html.Attributes.class "description" ]
    [ Html.text puzzle.description ]
  ]


buttonPlay : Signal.Address Action -> Model -> Html.Html
buttonPlay address puzzle =
  Html.button
    [ Html.Events.onClick address <| Select puzzle
    , Html.Attributes.class "fa fa-play btn-select-level" ]
    []
