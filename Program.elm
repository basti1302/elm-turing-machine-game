module Program (Model, init) where -- , Action, update, view) where

import Move exposing (Move)
import Symbol exposing (Symbol)
import State exposing (State)


-- A Dict (State, Symbol) (Symbol, State, Move)
-- would be much nicer but user defined types are not comparable, nor
-- are tuples of user defined types. See
-- https://github.com/elm-lang/elm-compiler/issues/774 and
-- https://github.com/elm-lang/elm-compiler/issues/1008
type alias Model = List (State, Symbol, Symbol, State, Move)


{-
This is the actual Turing Machine program.
We need to get this as user input later.

This one is the 3-state/2-symbol busy beaver.
-}
init : Model
init =
  [ (State.A, Symbol.Empty, Symbol.A, State.B,    Move.Right)
  , (State.A, Symbol.A,     Symbol.A, State.C,    Move.Left)
  , (State.B, Symbol.Empty, Symbol.A, State.A,    Move.Left)
  , (State.B, Symbol.A,     Symbol.A, State.B,    Move.Right)
  , (State.C, Symbol.Empty, Symbol.A, State.B,    Move.Left)
  , (State.C, Symbol.A,     Symbol.A, State.HALT, Move.Right)
  ]
