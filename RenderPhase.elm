module RenderPhase where

import Move exposing (Move)
import State exposing (State)
import Symbol exposing (Symbol)

type RenderPhase =
    Init
  | WriteSymbol Symbol
  | StartTransition (State, Symbol, Move)
  | CompleteStep
