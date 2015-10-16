module Game.Instruction
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
import List.Extra

import Game.Move as Move exposing (Move)
import Game.Symbol as Symbol exposing (Symbol)
import Game.State as State exposing (State)


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
    ChangeStateOut (List State)
  | ChangeSymbolOut (List Symbol)
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
    ChangeStateOut possibleStates ->
      let state' = shiftWithDefault State.HALT output.state possibleStates
      in { instruction | output <- { output | state <- state' }}
    ChangeSymbolOut possibleSymbols ->
      let symbol' = shiftWithDefault Symbol.Empty output.symbol possibleSymbols
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
    [ Html.Attributes.class
        ("symbol-" ++ Symbol.toColor instruction.input.symbol)
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

viewOutputState : List State -> Signal.Address Action -> Model -> Html.Html
viewOutputState possibleStates address instruction =
  Html.td
    [ Html.Events.onClick address (ChangeStateOut possibleStates)
    , Html.Attributes.class "state " ]
    [ let class = "fa " ++ State.toClass instruction.output.state
      in Html.span [Html.Attributes.class class] []
    ]

viewOutputSymbol : List Symbol -> Signal.Address Action -> Model -> Html.Html
viewOutputSymbol possibleSymbols address instruction =
  Html.td
    [ Html.Events.onClick address (ChangeSymbolOut possibleSymbols)
    , Html.Attributes.class
        ("symbol-" ++ Symbol.toColor instruction.output.symbol)
    , Html.Attributes.style
      [("background-color", Symbol.toColor instruction.output.symbol)]
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


{-
Looks for the given element in the list and returns the right neighbour of it.
-}
shiftWithDefault : a -> a -> List a -> a
shiftWithDefault default elem list =
  shift elem list |> Maybe.withDefault default


{-
Looks for the given element in the list and returns the right neighbour of it.
-}
shift : a -> List a -> Maybe a
shift elem list =
  let
    maybeIndex = List.Extra.elemIndex elem list
    remainder = case maybeIndex of
      Just idx -> List.drop (idx + 1) list
      Nothing -> []
  in
    if List.isEmpty remainder
      then List.head list
      else List.head remainder
