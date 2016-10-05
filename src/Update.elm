module Update exposing (update)

import Model exposing (newGame, generateNewFlag, flagDatabase, timePerQuestion, Event(..), Model(..), ActiveGameState, DifficultyLevel(..))
import Task
import Time exposing (Time)
import Dict exposing (Dict)
import String
import Process

update : Event -> Model -> (Model, Cmd Event)
update event model =
  case model of
    ActiveGame state -> updateActiveGame event state
    StartMenu state ->
      case event of
        Start -> (newGame state.difficultyLevel, generateNewFlag state.difficultyLevel)
        ChangeDifficulty level -> (StartMenu { difficultyLevel = level }, Cmd.none)
        Tick _ -> (model, Cmd.none)
        _ -> unexpectedEvent event
    GameOverMenu state ->
      case event of
        Start -> (newGame Level1, generateNewFlag Level1)
        Tick _ -> (model, Cmd.none)
        _ -> unexpectedEvent event

updateActiveGame : Event -> ActiveGameState -> (Model, Cmd Event)
updateActiveGame event state =
  let
      onSubmit state =
        let
            equal maybe value = (Maybe.map ((==) value) maybe) |> Maybe.withDefault False
            mungedInput = String.toLower state.currentInput
            flag = Dict.get mungedInput (flagDatabase state.difficultyLevel)
            isMatch = flag `equal` state.flagInfo.flag
        in
           if isMatch then
              (ActiveGame { state |  points = state.points + 1, currentInput = "", lastWrongQuestion = Nothing, time = timePerQuestion }, generateNewFlag state.difficultyLevel)
           else
              (ActiveGame state, Cmd.none)
      onTick state dt =
        if state.time > 0 then
          (ActiveGame { state | time = state.time - dt }, Cmd.none)
        else
          (GameOverMenu { points = state.points }, Cmd.none)
  in
    case event of
      Tick dt -> onTick state dt
      NewInput input -> (ActiveGame { state | currentInput = input }, Cmd.none)
      Submit -> onSubmit state
      RemoveWrongAnswer -> (ActiveGame { state | lastWrongQuestion = Nothing }, Cmd.none)
      NewFlag flagInfo -> (ActiveGame { state | flagInfo = flagInfo }, Cmd.none)
      Skip countryName -> (ActiveGame { state | lastWrongQuestion = Just countryName, time = timePerQuestion }, Cmd.batch [(generateNewFlag state.difficultyLevel), eventuallyRemoveWrongAnswer])
      _ -> unexpectedEvent event

unexpectedEvent : Event -> a
unexpectedEvent event = Debug.crash ("Unexpected event " ++ (toString event))

eventuallyRemoveWrongAnswer : Cmd Event
eventuallyRemoveWrongAnswer = waitThen (3 * Time.second) RemoveWrongAnswer

waitThen : Time -> a -> Cmd a
waitThen time msg =
  let wrap thing = (\_ -> thing)
      sleep = Process.sleep time `Task.andThen` wrap (Task.succeed ())
  in Task.perform (wrap msg) (wrap msg) sleep
