module ProgramRow (Model, init, toQuintupel, Action, update, view) where

import Html
import Html.Attributes
import Html.Events

import Move exposing (Move)
import Symbol exposing (Symbol)
import State exposing (State)


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
  , symbolOut = Symbol.blank
  , move = Move.Right
  }


toQuintupel : Model -> (State, Symbol, Symbol, State, Move)
toQuintupel row =
  (row.stateIn, row.symbolIn, row.symbolOut, row.stateOut, row.move)

update : Action -> Model -> Model
update action model =
  case action of
    ChangeStateOut ->
      let stateOut' =
        case model.stateOut of
          State.A -> State.B
          State.B -> State.C
          State.C -> State.HALT
          State.HALT -> State.A
      in { model | stateOut <- stateOut' }
    ChangeSymbolOut ->
      let symbolOut' =
        case model.symbolOut of
          Symbol.Empty -> Symbol.A
          Symbol.A -> Symbol.Empty
      in { model | symbolOut <- symbolOut' }
    ChangeMove ->
      let move' =
        case model.move of
          Move.Left -> Move.Right
          Move.Right -> Move.Left
      in { model | move <- move' }


view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.tr []
    [ Html.td
      [ Html.Attributes.class "state" ]
      [ let class = "fa " ++ State.toClass model.stateIn
        in Html.span [Html.Attributes.class class] []
      ]
    , Html.td
      [ Html.Attributes.class "symbol"
      , Html.Attributes.style
        [( "background-color" , Symbol.toColor model.symbolIn
        )]
      ]
      []
    , Html.td
      [ Html.Attributes.class "spacer" ]
      []
    , Html.td
      [ Html.Events.onClick address ChangeStateOut
      , Html.Attributes.class "state " ]
      [ let class = "fa " ++ State.toClass model.stateOut
        in Html.span [Html.Attributes.class class] []
      ]
    , Html.td
      [ Html.Events.onClick address ChangeSymbolOut
      , Html.Attributes.class "symbol"
      , Html.Attributes.style
        [( "background-color" , Symbol.toColor model.symbolOut
        )]
      ]
      []
    , Html.td
      [ Html.Events.onClick address ChangeMove
      , Html.Attributes.class "move"
      ]
      [ let class = case model.move of
          Move.Left -> "fa fa-arrow-circle-left"
          Move.Right -> "fa fa-arrow-circle-right"
        in Html.span [Html.Attributes.class class] []
      ]
    ]
