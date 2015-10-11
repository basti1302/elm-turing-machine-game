module Game.State where

type State = HALT | A | B | C | D | E | F | G

toClass : State -> String
toClass state =
  case state of
    A    -> "fa-circle-thin"
    B    -> "fa-dot-circle-o"
    C    -> "fa-circle"
    D    -> "fa-transgender"
    E    -> "fa-diamond"
    F    -> "fa-cube"
    G    -> "fa-cog"
    HALT -> "fa-pause"

