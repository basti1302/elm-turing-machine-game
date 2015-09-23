module Cell where

import Symbol exposing (..)

type alias Cell = { symbol : Symbol }

cell : Symbol -> Cell
cell symbol = { symbol = symbol }

blankCell: Cell
blankCell = { symbol = blank }
