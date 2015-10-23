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


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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


viewInputState : Model -> Html
viewInputState instruction =
  td
    [ class "state" ]
    [ let clazz = "fa " ++ State.toClass instruction.input.state
      in span [class clazz] []
    ]

viewInputSymbol : Model -> Html
viewInputSymbol instruction =
  td
    [ class
        ("symbol-" ++ Symbol.toColor instruction.input.symbol)
    ]
    []

spacer : Html
spacer =
  td
    [ class "spacer" ]
    []

viewOutputState : List State -> Signal.Address Action -> Model -> Html
viewOutputState possibleStates address instruction =
  td
    [ onClick address (ChangeStateOut possibleStates)
    , class "output state " ]
    [ let clazz = "fa " ++ State.toClass instruction.output.state
      in span [class clazz] []
    ]

viewOutputSymbol : List Symbol -> Signal.Address Action -> Model -> Html
viewOutputSymbol possibleSymbols address instruction =
  td
    [ onClick address (ChangeSymbolOut possibleSymbols)
    , class
        ("output symbol-" ++ Symbol.toColor instruction.output.symbol)
    ]
    []


viewMove : Signal.Address Action -> Model -> Html
viewMove address instruction =
  td
    [ onClick address ChangeMove
    , class "move"
    ]
    [
      let clazz =
        if instruction.output.state == State.HALT then "fa"
        else case instruction.output.move of
          Move.Left -> "fa fa-arrow-circle-left"
          Move.Right -> "fa fa-arrow-circle-right"
      in span [class clazz] []
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
