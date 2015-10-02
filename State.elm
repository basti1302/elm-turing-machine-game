module State where

type State = HALT | A | B | C | D | E | F | G

toClass : State -> String
toClass state =
  case state of
    A    -> "fa-circle-thin"
    B    -> "fa-circle"
    C    -> "fa-dot-circle-o"
    D    -> "fa-cog"
    E    -> "fa-transgender"
    F    -> "fa-diamond"
    G    -> "fa-cube"
    HALT -> "fa-pause"

