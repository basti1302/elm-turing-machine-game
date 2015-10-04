module Instruction
  ( Input
  , Output
  , Model
  , fromInput
  , fromQuintuple
  , Action
  , update
  , viewInputState
  , viewInputSymbol
  , spacer
  , viewOutputState
  , viewOutputSymbol
  , viewMove
  ) where


import Html
import Html.Attributes
import Html.Events

import Move exposing (Move)
import Symbol exposing (Symbol)
import State exposing (State)


type alias Input =
  { state : State
  , symbol : Symbol
  }


type alias Output =
  { state : State
  , symbol : Symbol
  , move : Move
  }


type alias Model =
  { input : Input
  , output : Output
  }


type Action =
    ChangeStateOut
  | ChangeSymbolOut
  | ChangeMove


fromInput : State -> Symbol -> Model
fromInput stateIn symbolIn =
  { input =
    { state = stateIn
    , symbol = symbolIn
    }
  , output =
    { state = State.A
    , symbol = Symbol.blank
    , move = Move.Right
    }
  }


fromQuintuple : State -> Symbol -> State -> Symbol -> Move -> Model
fromQuintuple stateIn symbolIn stateOut symbolOut move =
  { input =
    { state = stateIn
    , symbol = symbolIn
    }
  , output =
    { state = stateOut
    , symbol = symbolOut
    , move = move
    }
  }


update : Action -> Model -> Model
update action instruction =
  let output = instruction.output
  in case action of
    ChangeStateOut ->
      let state' = case output.state of
        State.A -> State.B
        State.B -> State.C
        State.C -> State.HALT
        State.HALT -> State.A
      in { instruction | output <- { output | state <- state' }}
    ChangeSymbolOut ->
      let symbol' = case output.symbol of
        Symbol.Empty -> Symbol.A
        Symbol.A -> Symbol.Empty
      in { instruction | output <- { output | symbol <- symbol' }}
    ChangeMove ->
      let move' = case output.move of
        Move.Left -> Move.Right
        Move.Right -> Move.Left
      in { instruction | output <- { output | move <- move' }}


viewInputState : Model -> Html.Html
viewInputState instruction =
  Html.td
    [ Html.Attributes.class "state" ]
    [ let class = "fa " ++ State.toClass instruction.input.state
      in Html.span [Html.Attributes.class class] []
    ]

viewInputSymbol : Model -> Html.Html
viewInputSymbol instruction =
  Html.td
    [ Html.Attributes.class "symbol"
    , Html.Attributes.style
      [( "background-color" , Symbol.toColor instruction.input.symbol
      )]
    ]
    []

spacer : Html.Html
spacer =
  Html.td
    [ Html.Attributes.class "spacer" ]
    []

viewOutputState : Signal.Address Action -> Model -> Html.Html
viewOutputState address instruction =
  Html.td
    [ Html.Events.onClick address ChangeStateOut
    , Html.Attributes.class "state " ]
    [ let class = "fa " ++ State.toClass instruction.output.state
      in Html.span [Html.Attributes.class class] []
    ]

viewOutputSymbol : Signal.Address Action -> Model -> Html.Html
viewOutputSymbol address instruction =
  Html.td
    [ Html.Events.onClick address ChangeSymbolOut
    , Html.Attributes.class "symbol"
    , Html.Attributes.style
      [( "background-color" , Symbol.toColor instruction.output.symbol
      )]
    ]
    []

viewMove : Signal.Address Action -> Model -> Html.Html
viewMove address instruction =
  Html.td
    [ Html.Events.onClick address ChangeMove
    , Html.Attributes.class "move"
    ]
    [ let class = case instruction.output.move of
        Move.Left -> "fa fa-arrow-circle-left"
        Move.Right -> "fa fa-arrow-circle-right"
      in Html.span [Html.Attributes.class class] []
    ]
