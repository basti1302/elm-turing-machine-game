module Game.Symbol where

type Symbol = Empty | A | B | C | D | E | F | G

blank : Symbol
blank = Empty

toColor : Symbol -> String
toColor symbol =
  case symbol of
    Empty -> "black"
    A     -> "red"
    B     -> "blue"
    C     -> "green"
    D     -> "cyan"
    E     -> "magenta"
    F     -> "yellow"
    G     -> "white"
