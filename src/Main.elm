import Time exposing (Time)
import Task
import Process
import Random
import String
import Array
import Dict exposing (Dict)
import Json.Decode exposing ((:=))
import Html exposing (Html, div, span, h1, text, p, input, form, button, fieldset, label)
import Html.Events exposing (on, onInput, onClick)
import Html.Attributes exposing (style, placeholder, value, type', name, checked)
import Html.App as App
import Model exposing (..)
import Data exposing (..)

main : Program Never
main = App.program { init = (init, Cmd.none), update = update, view = view, subscriptions = subscription }

generateNewFlag : DifficultyLevel -> Cmd Event
generateNewFlag difficultyLevel = Random.generate NewFlag (newFlagGenerator difficultyLevel)

newFlagGenerator : DifficultyLevel -> Random.Generator FlagInfo
newFlagGenerator difficultyLevel =
  flagDatabase difficultyLevel |> getRandom |> Random.map (\(countryName, flag) -> { flag = flag, countryName = countryName })

flagDatabase : DifficultyLevel -> Dict String String
flagDatabase difficultyLevel =
  case difficultyLevel of
    Level1 -> level1
    Level2 -> level2
    Level3 -> level3
    Level4 -> level4
    Level5 -> level5

getRandom : Dict comparable a -> Random.Generator (comparable, a)
getRandom dict = Random.map (atIndex dict) (randomIndex dict)

randomIndex : Dict comparable a -> Random.Generator Int
randomIndex map =
  let lastIndex = (List.length (Dict.keys map)) - 1
  in Random.int 0 lastIndex

atIndex : Dict comparable a -> Int -> (comparable, a)
atIndex dict i = case dict |> Dict.toList |> Array.fromList |> Array.get i of
  Just item -> item
  Nothing -> Debug.crash "Index out of range"

contains : Maybe a -> a -> Bool
contains maybe value = (Maybe.map (\v -> value == v) maybe) |> Maybe.withDefault False

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

eventuallyRemoveWrongAnswer : Cmd Event
eventuallyRemoveWrongAnswer = waitThen (3 * Time.second) RemoveWrongAnswer

waitThen : Time -> a -> Cmd a
waitThen time msg =
  let wrap thing = (\_ -> thing)
      sleep = Process.sleep time `Task.andThen` wrap (Task.succeed ())
  in Task.perform (wrap msg) (wrap msg) sleep

updateActiveGame : Event -> ActiveGameState -> (Model, Cmd Event)
updateActiveGame event state =
  let
      onSubmit state =
        let
            mungedInput = String.toLower state.currentInput
            flag = Dict.get mungedInput (flagDatabase state.difficultyLevel)
            isMatch = contains flag state.flagInfo.flag
        in
           if isMatch then
              (ActiveGame { state |  points = state.points + 1, currentInput = "", lastWrongQuestion = Nothing }, generateNewFlag state.difficultyLevel)
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
      Skip countryName -> (ActiveGame { state | lastWrongQuestion = Just countryName }, Cmd.batch [(generateNewFlag state.difficultyLevel), eventuallyRemoveWrongAnswer])
      _ -> unexpectedEvent event

unexpectedEvent : Event -> a
unexpectedEvent event = Debug.crash ("Unexpected event " ++ (toString event))

tickRate : Time
tickRate = 100 * Time.millisecond

radio : String -> Bool -> Event -> Html Event
radio value isChecked msg =
  label
    [ style [("padding", "20px")]
    ]
    [ input [ type' "checkbox", checked isChecked, name "font-size", onClick msg ] []
    , text value
    ]

view : Model -> Html Event
view model =
  case model of
    ActiveGame state -> activeGame state
    StartMenu state -> startMenu state
    GameOverMenu state -> gameOver state

startMenu : StartMenuState -> Html Event
startMenu state =
  div []
    [ h1 [] [text "Flags"]
    , fieldset []
        [ radio "Xenophobe" (state.difficultyLevel == Level1) (ChangeDifficulty Level1)
        , radio "Resort Tourist" (state.difficultyLevel == Level2) (ChangeDifficulty Level2)
        , radio "European Backpacker" (state.difficultyLevel == Level3) (ChangeDifficulty Level3)
        , radio "National Geographic Enthusiast" (state.difficultyLevel == Level4) (ChangeDifficulty Level4)
        , radio "Professor of Geography" (state.difficultyLevel == Level5) (ChangeDifficulty Level5)
        ]
    , button [onClick Start] [text "Begin"]
    ]

activeGame : ActiveGameState -> Html Event
activeGame state =
  div []
    [ title
    , points state.points
    , lastWrongQuestion state.lastWrongQuestion
    , time state.time
    , flag state.flagInfo.flag
    , answer state.currentInput
    , skipButton state.flagInfo.countryName
    ]

lastWrongQuestion : Maybe String -> Html a
lastWrongQuestion country =
  case country of
    Just c -> div [] [text ("It was " ++ c)]
    Nothing -> span [] []


gameOver : GameOverState -> Html Event
gameOver state = div []
  [ div [style [("font-size", "24px"), ("text-align", "center")]] [text "Over"]
  , points state.points
  , button [style [("font-size", "24px"), ("display", "block"), ("margin", "0 auto")], onClick Start] [text "Restart"]
  ]

title : Html a
title = h1 [style [("font-size", "84px"), ("text-align", "center")]] [text "Flags"]

points : Int -> Html a
points value = p [style [("font-size", "24px"), ("text-align", "center")]] [text ("Points: " ++ (toString value))]

time : Time -> Html a
time time = div [] [text ("Time left: " ++ (timeString time) ++ "s")]

timeString : Time -> String
timeString time =
  let seconds = time |> Time.inSeconds |> toString
  in case String.split "." seconds of
    [firstHalf, _] -> String.padRight ((String.length firstHalf) + 3) '0' seconds
    [firstHalf] -> String.padRight ((String.length firstHalf) + 3) '0' (firstHalf ++ ".")
    _ -> seconds

flag : String -> Html a
flag currentFlag = div [style [("font-size", "84px"), ("text-align", "center")]] [text currentFlag]

answer : String -> Html Event
answer currentValue =
  let
    styles = style
      [ ("font-size", "40px")
      , ("margin", "20px auto")
      , ("display", "block")
      , ("height", "50px")
      , ("width", "400px")]
  in
    input
      [ styles
      , placeholder "Flag"
      , value currentValue
      , onInput NewInput
      , on "keydown" keyDownEvent ] []

keyDownEvent : Json.Decode.Decoder Event
keyDownEvent =
  let key = "key" := Json.Decode.string
      keyToEvent key = if key == "Enter" then Submit else Tick 0
  in Json.Decode.map keyToEvent key

skipButton : String -> Html Event
skipButton countryName =
  button [ style
            [ ("font-size", "34px")
            , ("margin", "auto")
            , ("display", "block")
            , ("height", "50px")
            , ("width", "150px")
            ]
         , onClick (Skip countryName)]
         [text "skip"]

subscription : Model -> Sub Event
subscription model = Time.every tickRate (\_ -> Tick tickRate)
