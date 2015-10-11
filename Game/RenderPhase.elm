module Game.RenderPhase where

import Game.Move as Move exposing (Move)
import Game.State as State exposing (State)
import Game.Symbol as Symbol exposing (Symbol)

type RenderPhase =
    Init
  | WriteSymbol Symbol
  | StartTransition (State, Symbol, Move)
  | CompleteStep
