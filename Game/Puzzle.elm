module Game.Puzzle
  ( Model
  , init
  , isSolved
  )
  where

import Game.Tape as Tape

type alias Model =
  { input : Tape.Model
  , result : Tape.Model
  -- , Tape Alphabet?
  -- , Set of possible states (as subset from Game.State)?
  }


init : Tape.Model -> Tape.Model -> Model
init input result =
  { input = input
  , result = result
  }


isSolved : Tape.Model -> Model -> Bool
isSolved tape puzzle =
  Tape.trim tape == Tape.trim puzzle.result
