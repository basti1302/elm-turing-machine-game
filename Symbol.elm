module Symbol where

type Symbol = Empty | A | B | C | D | E | F | G

blank : Symbol
blank = Empty

toColor : Symbol -> String
toColor symbol =
  case symbol of
    Empty -> "blue"
    A     -> "red"
    B     -> "green"
    C     -> "cyan"
    D     -> "magenta"
    E     -> "yellow"
    F     -> "black"
    G     -> "white"
