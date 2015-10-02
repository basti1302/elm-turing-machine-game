module ProgramRow (Model, init, Action, update, view) where

import Html
import Html.Events


import Move exposing (Move)
import Symbol exposing (Symbol)
import State exposing (..)


-- TODO Merge ProgramView.elm and Program.elm??
type alias Model =
  { stateIn : State
  , symbolIn : Symbol
  , stateOut : State
  , symbolOut : Symbol
  , move : Move
  }


type Action =
    ChangeStateOut
  | ChangeSymbolOut
  | ChangeMove


init : State -> Symbol -> Model
init stateIn symbolIn =
  { stateIn = stateIn
  , symbolIn = symbolIn
  , stateOut = State.A
  , symbolOut = Symbol.White
  , move = Move.Right
  }


update : Action -> Model -> Model
update action model =
  case action of
    ChangeStateOut ->
      let stateOut' =
        case model.stateOut of
          A -> B
          B -> C
          C -> HALT
          HALT -> A
      in { model | stateOut <- stateOut' }
    ChangeSymbolOut ->
      let symbolOut' =
        case model.symbolOut of
          Symbol.White -> Symbol.Black
          Symbol.Black -> Symbol.White
      in { model | symbolOut <- symbolOut' }
    ChangeMove ->
      let move' =
        case model.move of
          Move.Left -> Move.Right
          Move.Right -> Move.Left
      in { model | move <- move' }


view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div []
    [ Html.span
      []
      [ Html.text <| "[" ++ toString model.stateIn ++ ", " ]
    , Html.span
      []
      [ Html.text <| toString model.symbolIn ++ "]"]
    , Html.span
      []
      [ Html.text <| " => " ]
    , Html.span
      [ Html.Events.onClick address ChangeStateOut ]
      [ Html.text <| "[" ++ toString model.stateOut ++ ", "]
    , Html.span
      [ Html.Events.onClick address ChangeSymbolOut ]
      [ Html.text <| toString model.symbolOut ++ ", "]
    , Html.span
      [ Html.Events.onClick address ChangeMove ]
      [ Html.text <| toString model.move ++ "]"]
    ]


{-
main : Signal Html.Html
main =
  StartApp.start { model = init, view = view, update = update }
-}
