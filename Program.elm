module Program (Model, init) where -- , Action, update, view) where

import Move exposing (Move)
import Symbol exposing (Symbol)
import State exposing (..)


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
  [ (A, Symbol.White, Symbol.Black, B, Move.Right)
  , (A, Symbol.Black, Symbol.Black, C, Move.Left)
  , (B, Symbol.White, Symbol.Black, A, Move.Left)
  , (B, Symbol.Black, Symbol.Black, B, Move.Right)
  --, (C, Symbol.White, Symbol.Black, B, Move.Left)
  , (C, Symbol.Black, Symbol.Black, HALT, Move.Right)
  ]
