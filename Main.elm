module Main (main) where

import Html
import Html.Attributes
import Signal exposing (Signal, (<~), (~))
import Time exposing (every, millisecond)

import Game.MachineView as MachineView
import Game.Program as Program
import Game.Puzzle as Puzzle
import Game.Puzzles as Puzzles
import Game.Screen as Screen exposing (Screen)
import Game.SelectLevel as SelectLevel


type alias Model =
  { machineView : MachineView.Model
  , program : Program.Model
  , puzzle : Puzzle.Model
  , selectLevel : SelectLevel.Model
  }


type alias Context =
  { view : Screen
  , hasWon : Bool
  , hasLost : Bool
  }


type Action
  = SelectLevelAction SelectLevel.Action
  | MachineAction MachineView.Action
  | ProgramAction Program.Action


{-|
Initializes the model and the context.
-}
init : (Model, Context)
init =
  ({ machineView = MachineView.initEmpty
   , program = Program.init Puzzles.default
   , puzzle = Puzzles.default
   , selectLevel = SelectLevel.init
   },
   { view = Screen.SelectLevel
   , hasWon = False
   , hasLost = False
   })


{-|
Updates the current view on each signal tick.
-}
update : Action -> (Model, Context) -> (Model, Context)
update action (game, context) =
  case action of
    SelectLevelAction selectLevelAction ->
      if context.view /= Screen.SelectLevel
        then (game, context) -- discard select level events if not in machine view
        else updateSelectLevel selectLevelAction (game, context)
    MachineAction machineAction ->
      if context.view /= Screen.Machine
        then (game, context) -- do not run machine program if not in machine view
        else updateMachine machineAction (game, context)
    ProgramAction programAction ->
      if context.view /= Screen.Program
        then (game, context) -- do not execute program actions if not in program view
        else updateProgram programAction (game, context)


updateSelectLevel : SelectLevel.Action -> (Model, Context) -> (Model, Context)
updateSelectLevel selectLevelAction (game, context) =
  case selectLevelAction of
    SelectLevel.Select puzzle ->
      ({ game |
           puzzle <- puzzle,
           program <- Program.init puzzle
       },
       { context | view <- Screen.Program })
    otherwise ->
      (game, context)


updateMachine : MachineView.Action -> (Model, Context) -> (Model, Context)
updateMachine machineAction (game, context) =
  case machineAction of
    MachineView.SwitchToProgram ->
      (game, { context | view <- Screen.Program })
    MachineView.SwitchToLevelSelect ->
      (game, { context | view <- Screen.SelectLevel })
    otherwise ->
      let
        (machine', renderPhase') = MachineView.update machineAction game.machineView
        hasWon' =
          machine'.stopped &&
          Puzzle.isSolved machine'.tape game.puzzle
        hasLost' =
          machine'.stopped &&
          not (Puzzle.isSolved machine'.tape game.puzzle)
      in
        ({ game | machineView <- (machine', renderPhase') },
         { context | hasWon <- hasWon'
                   , hasLost <- hasLost' })


updateProgram : Program.Action -> (Model, Context) -> (Model, Context)
updateProgram programAction (game, context) =
  case programAction of
    Program.SwitchToMachine ->
      ({game |
           machineView <-
           MachineView.init game.puzzle game.program
       }
      , { context | view <- Screen.Machine })
    Program.SwitchToLevelSelect ->
      (game, { context | view <- Screen.SelectLevel })
    otherwise ->
      ({ game |
         program <- Program.update programAction game.program },
         context
      )


{-|
Renders the game view.
-}
view : Signal.Address Action -> (Model, Context) -> Html.Html
view address (game, context) =
  let content =
    if context.hasWon then [ Html.text "YOU WON!" ]
    else if context.hasLost then [ Html.text "nope :-(" ]
    else
      case context.view of
        Screen.SelectLevel ->
          [ SelectLevel.view (Signal.forwardTo address SelectLevelAction) game.selectLevel ]
        Screen.Program ->
          [ Program.view (Signal.forwardTo address ProgramAction) game.program ]
        Screen.Machine ->
          [ MachineView.view (Signal.forwardTo address MachineAction) game.machineView ]
  in
    Html.div
      [ Html.Attributes.class "game" ]
      content


{-|
Generates a tick every 300 milliseconds that is used to execute a machineProgram.
-}
tickSignal : Signal Action
tickSignal =
  (\ _ -> (MachineAction MachineView.ExecuteMachineStep))
  <~ (every <| 300 * millisecond)


{-|
The main mailbox that routes the signals from all UI input elements.
-}
mainMailbox : Signal.Mailbox Action
mainMailbox = Signal.mailbox (MachineAction MachineView.ExecuteMachineStep)


{-|
Merges the constant timed tick and the signal from UI input elements.
-}
mainSignal : Signal Action
mainSignal = Signal.merge mainMailbox.signal tickSignal


{-|
The HTML output signal.
-}
main : Signal Html.Html
main =
  let
    model =
      Signal.foldp
        update
        init
        mainSignal
  in
    (view mainMailbox.address) <~ model
