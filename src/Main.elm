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

type DifficultyLevel = Level1 | Level2 | Level3 | Level4 | Level5
type alias FlagInfo = { countryName: String, flag: String }
type Event = NewInput String | Submit | NewFlag FlagInfo | Skip String | Tick Time | Start | ChangeDifficulty DifficultyLevel | RemoveWrongAnswer
type Model = StartMenu StartMenuState | ActiveGame ActiveGameState | GameOverMenu GameOverState
type alias StartMenuState = { difficultyLevel: DifficultyLevel }
type alias ActiveGameState =
  { points: Int
  , currentFlag: String
  , currentCountryName: String
  , currentInput: String
  , time: Time
  , difficultyLevel: DifficultyLevel
  , lastWrongQuestion: Maybe String
  }
type alias GameOverState = { points: Int }

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
            isMatch = contains flag state.currentFlag
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
      NewFlag flagInfo -> (ActiveGame { state | currentFlag = flagInfo.flag, currentCountryName = flagInfo.countryName }, Cmd.none)
      Skip countryName -> (ActiveGame { state | lastWrongQuestion = Just countryName }, Cmd.batch [(generateNewFlag state.difficultyLevel), eventuallyRemoveWrongAnswer])
      _ -> unexpectedEvent event

newGame : DifficultyLevel -> Model
newGame difficultyLevel = ActiveGame
  { difficultyLevel = difficultyLevel
  , points = 0
  , currentFlag = germany
  , currentCountryName = "germany"
  , time = 15 * Time.second
  , currentInput = ""
  , lastWrongQuestion = Nothing
  }

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
    , flag state.currentFlag
    , answer state.currentInput
    , skipButton state.currentCountryName
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

init : Model
init = StartMenu { difficultyLevel = Level1 }

china = (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1F3')
germany = (String.fromChar '\x1F1E9') ++ (String.fromChar '\x1F1EA')
spain = (String.fromChar '\x1F1EA') ++ (String.fromChar '\x1F1F8')
france = (String.fromChar '\x1F1EB') ++ (String.fromChar '\x1F1F7')
uk = (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1E7')
usa = (String.fromChar '\x1F1FA') ++ (String.fromChar '\x1F1F8')

level1 : Dict String String
level1 = Dict.fromList group1

level2 : Dict String String
level2 = Dict.union (Dict.fromList group1) (Dict.fromList group2)

level3 : Dict String String
level3 = Dict.union (Dict.fromList group2) (Dict.fromList group3)

level4 : Dict String String
level4 = Dict.union (Dict.fromList group3) (Dict.fromList group4)

level5 : Dict String String
level5 = Dict.union (Dict.fromList group4) (Dict.fromList group5)

group1 : List (String, String)
group1 =
  [ ("china",  china)
  , ("germany",  germany)
  , ("spain",  spain)
  , ("france", france)
  , ("uk",  uk)
  , ("united kingdom",  uk)
  , ("great britain",  uk)
  , ("italy",  (String.fromChar '\x1F1EE') ++ (String.fromChar '\x1F1F9'))
  , ("japan",  (String.fromChar '\x1F1EF') ++ (String.fromChar '\x1F1F5'))
  , ("south korea",  (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1F7'))
  , ("russia",  (String.fromChar '\x1F1F7') ++ (String.fromChar '\x1F1FA'))
  , ("united states",  usa)
  , ("usa", usa)
  , ("us", usa)
  , ("austria", (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1F9'))
  , ("australia", (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1FA'))
  , ("belgium", (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1EA'))
  , ("brazil", (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1F7'))
  , ("switzerland", (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1ED'))
  , ("canada", (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1E6'))
  , ("denmark", (String.fromChar '\x1F1E9') ++ (String.fromChar '\x1F1F0'))
  , ("finland", (String.fromChar '\x1F1EB') ++ (String.fromChar '\x1F1EE'))
  , ("greece", (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1F7'))
  , ("lebanon", (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1E7'))
  , ("mexico", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1FD'))
  , ("jamaica", (String.fromChar '\x1F1EF') ++ (String.fromChar '\x1F1F2'))
  , ("netherlands", (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1F1'))
  , ("norway", (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1F4'))
  , ("poland", (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1F1'))
  , ("ireland", (String.fromChar '\x1F1EE') ++ (String.fromChar '\x1F1EA'))
  , ("israel", (String.fromChar '\x1F1EE') ++ (String.fromChar '\x1F1F1'))
  , ("india", (String.fromChar '\x1F1EE') ++ (String.fromChar '\x1F1F3'))
  , ("portugal", (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1F9'))
  , ("turkey", (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1F7'))
  , ("sweden", (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1EA'))
  ]

group2 : List (String, String)
group2 =
  [ ("iraq", (String.fromChar '\x1F1EE') ++ (String.fromChar '\x1F1F6'))
  , ("croatia", (String.fromChar '\x1F1ED') ++ (String.fromChar '\x1F1F7'))
  , ("costa rica", (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1F7'))
  , ("chile", (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1F1'))
  , ("cuba", (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1FA'))
  , ("czech republic", (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1FF'))
  , ("egypt", (String.fromChar '\x1F1EA') ++ (String.fromChar '\x1F1EC'))
  , ("greenland", (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1F1'))
  , ("hong kong", (String.fromChar '\x1F1ED') ++ (String.fromChar '\x1F1F0'))
  , ("puerto rico", (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1F7'))
  , ("iran", (String.fromChar '\x1F1EE') ++ (String.fromChar '\x1F1F7'))
  , ("iceland", (String.fromChar '\x1F1EE') ++ (String.fromChar '\x1F1F8'))
  , ("north korea", (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1F5'))
  , ("pakistan", (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1F0'))
  , ("ukraine", (String.fromChar '\x1F1FA') ++ (String.fromChar '\x1F1E6'))
  , ("uganda", (String.fromChar '\x1F1FA') ++ (String.fromChar '\x1F1EC'))
  , ("uruguay", (String.fromChar '\x1F1FA') ++ (String.fromChar '\x1F1FE'))
  , ("venezuela", (String.fromChar '\x1F1FB') ++ (String.fromChar '\x1F1EA'))
  , ("south africa", (String.fromChar '\x1F1FF') ++ (String.fromChar '\x1F1E6'))
  , ("vatican city", (String.fromChar '\x1F1FB') ++ (String.fromChar '\x1F1E6'))
  , ("argentina", (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1F7'))
  , ("colombia", (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1F4'))
  , ("bulgaria", (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1EC'))
  , ("hungary", (String.fromChar '\x1F1ED') ++ (String.fromChar '\x1F1FA'))
  , ("algeria", (String.fromChar '\x1F1E9') ++ (String.fromChar '\x1F1FF'))
  , ("cyprus", (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1FE'))
  ]

group3 : List (String, String)
group3 =
  [ ("andorra", (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1E9'))
  , ("united arab emirates", (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1EA'))
  , ("afghanistan", (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1EB'))
  , ("albania", (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1F1'))
  , ("armenia", (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1F2'))
  , ("angola", (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1F4'))
  , ("bosnia and herzegovina", (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1E6'))
  , ("barbados", (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1E7'))
  , ("bangladesh", (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1E9'))
  , ("bolivia", (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1F4'))
  , ("botswana", (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1FC'))
  , ("belarus", (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1FE'))
  , ("belize", (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1FF'))
  , ("congo", (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1E9'))
  , ("congo", (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1EC'))
  , ("côte d'ivoire", (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1EE'))
  , ("cameroon", (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1F2'))
  , ("gibraltar", (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1EE'))
  , ("ghana", (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1ED'))
  , ("georgia", (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1EA'))
  , ("dominican republic", (String.fromChar '\x1F1E9') ++ (String.fromChar '\x1F1F4'))
  , ("ecuador", (String.fromChar '\x1F1EA') ++ (String.fromChar '\x1F1E8'))
  , ("estonia", (String.fromChar '\x1F1EA') ++ (String.fromChar '\x1F1EA'))
  , ("honduras", (String.fromChar '\x1F1ED') ++ (String.fromChar '\x1F1F3'))
  , ("guatemala", (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1F9'))
  , ("haiti", (String.fromChar '\x1F1ED') ++ (String.fromChar '\x1F1F9'))
  , ("indonesia", (String.fromChar '\x1F1EE') ++ (String.fromChar '\x1F1E9'))
  , ("jordan", (String.fromChar '\x1F1EF') ++ (String.fromChar '\x1F1F4'))
  , ("kenya", (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1EA'))
  , ("cambodia", (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1ED'))
  , ("liechtenstein", (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1EE'))
  , ("sri lanka", (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1F0'))
  , ("liberia", (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1F7'))
  , ("macedonia", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1F0'))
  , ("philippines", (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1ED'))
  , ("romania", (String.fromChar '\x1F1F7') ++ (String.fromChar '\x1F1F4'))
  , ("serbia", (String.fromChar '\x1F1F7') ++ (String.fromChar '\x1F1F8'))
  , ("rwanda", (String.fromChar '\x1F1F7') ++ (String.fromChar '\x1F1FC'))
  , ("saudi arabia", (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1E6'))
  , ("sudan", (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1E9'))
  , ("singapore", (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1EC'))
  , ("uzbekistan", (String.fromChar '\x1F1FA') ++ (String.fromChar '\x1F1FF'))
  , ("isle of man", (String.fromChar '\x1F1EE') ++ (String.fromChar '\x1F1F2'))
  , ("thailand", (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1ED'))
  , ("viet nam", (String.fromChar '\x1F1FB') ++ (String.fromChar '\x1F1F3'))
  , ("zambia", (String.fromChar '\x1F1FF') ++ (String.fromChar '\x1F1F2'))
  , ("zimbabwe", (String.fromChar '\x1F1FF') ++ (String.fromChar '\x1F1FC'))
  , ("syrian arab republic", (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1FE'))
  , ("el salvador", (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1FB'))
  , ("slovenia", (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1EE'))
  , ("slovakia", (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1F0'))
  , ("paraguay", (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1FE'))
  , ("qatar", (String.fromChar '\x1F1F6') ++ (String.fromChar '\x1F1E6'))
  , ("new zealand", (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1FF'))
  , ("bahamas", (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1F8'))
  ]

group4 : List (String, String)
group4 =
  [ ("antigua and barbuda", (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1EC'))
  , ("kyrgyzstan", (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1EC'))
  , ("antarctica", (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1F6'))
  , ("american samoa", (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1F8'))
  , ("aruba", (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1FC'))
  , ("azerbaijan", (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1FF'))
  , ("burkina faso", (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1EB'))
  , ("burundi", (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1EE'))
  , ("bermuda", (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1F2'))
  , ("central african republic", (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1EB'))
  , ("cook islands", (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1F0'))
  , ("cape verde", (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1FB'))
  , ("curaçao", (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1FC'))
  , ("djibouti", (String.fromChar '\x1F1E9') ++ (String.fromChar '\x1F1EF'))
  , ("dominica", (String.fromChar '\x1F1E9') ++ (String.fromChar '\x1F1F2'))
  , ("western sahara", (String.fromChar '\x1F1EA') ++ (String.fromChar '\x1F1ED'))
  , ("ethiopia", (String.fromChar '\x1F1EA') ++ (String.fromChar '\x1F1F9'))
  , ("fiji", (String.fromChar '\x1F1EB') ++ (String.fromChar '\x1F1EF'))
  , ("falkland islands (malvinas)", (String.fromChar '\x1F1EB') ++ (String.fromChar '\x1F1F0'))
  , ("micronesia", (String.fromChar '\x1F1EB') ++ (String.fromChar '\x1F1F2'))
  , ("faroe islands", (String.fromChar '\x1F1EB') ++ (String.fromChar '\x1F1F4'))
  , ("gabon", (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1E6'))
  , ("grenada", (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1E9'))
  , ("french guiana", (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1EB'))
  , ("guinea", (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1F3'))
  , ("equatorial guinea", (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1F6'))
  , ("guam", (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1FA'))
  , ("guyana", (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1FE'))
  , ("jersey", (String.fromChar '\x1F1EF') ++ (String.fromChar '\x1F1EA'))
  , ("kuwait", (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1FC'))
  , ("cayman islands", (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1FE'))
  , ("kazakhstan", (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1FF'))
  , ("lao people's democratic republic", (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1E6'))
  , ("saint lucia", (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1E8'))
  , ("lesotho", (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1F8'))
  , ("lithuania", (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1F9'))
  , ("luxembourg", (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1FA'))
  , ("latvia", (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1FB'))
  , ("libya", (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1FE'))
  , ("morocco", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1E6'))
  , ("monaco", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1E8'))
  , ("moldova", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1E9'))
  , ("montenegro", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1EA'))
  , ("saint martin (french part)", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1EB'))
  , ("madagascar", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1EC'))
  , ("marshall islands", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1ED'))
  , ("mali", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1F1'))
  , ("myanmar", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1F2'))
  , ("mongolia", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1F3'))
  , ("macao", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1F4'))
  , ("niger", (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1EA'))
  , ("martinique", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1F6'))
  , ("malta", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1F9'))
  , ("maldives", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1FB'))
  , ("malawi", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1FC'))
  , ("malaysia", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1FE'))
  , ("mozambique", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1FF'))
  , ("namibia", (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1E6'))
  , ("nigeria", (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1EC'))
  , ("nicaragua", (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1EE'))
  , ("nepal", (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1F5'))
  , ("oman", (String.fromChar '\x1F1F4') ++ (String.fromChar '\x1F1F2'))
  , ("panama", (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1E6'))
  , ("peru", (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1EA'))
  , ("french polynesia", (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1EB'))
  , ("papua new guinea", (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1EC'))
  , ("palestinian territory", (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1F8'))
  , ("solomon islands", (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1E7'))
  , ("seychelles", (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1E8'))
  , ("saint helena, ascension and tristan da cunha", (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1ED'))
  , ("sierra leone", (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1F1'))
  , ("san marino", (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1F2'))
  , ("senegal", (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1F3'))
  , ("somalia", (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1F4'))
  , ("suriname", (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1F7'))
  , ("south sudan", (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1F8'))
  , ("turkmenistan", (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1F2'))
  , ("tunisia", (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1F3'))
  , ("swaziland", (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1FF'))
  , ("chad", (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1E9'))
  , ("togo", (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1EC'))
  , ("tajikistan", (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1EF'))
  , ("tonga", (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1F4'))
  , ("trinidad and tobago", (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1F9'))
  , ("tanzania", (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1FF'))
  , ("samoa", (String.fromChar '\x1F1FC') ++ (String.fromChar '\x1F1F8'))
  , ("yemen", (String.fromChar '\x1F1FE') ++ (String.fromChar '\x1F1EA'))
  , ("taiwan", (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1FC'))
  , ("virgin islands, british", (String.fromChar '\x1F1FB') ++ (String.fromChar '\x1F1EC'))
  , ("virgin islands, u.s.", (String.fromChar '\x1F1FB') ++ (String.fromChar '\x1F1EE'))
  ]

group5 : List (String, String)
group5 =
  [ ("anguilla", (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1EE'))
  , ("åland islands", (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1FD'))
  , ("bahrain", (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1ED'))
  , ("benin", (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1EF'))
  , ("saint barthélemy", (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1F1'))
  , ("brunei darussalam", (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1F3'))
  , ("bonaire, sint eustatius and saba", (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1F6'))
  , ("bhutan", (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1F9'))
  , ("bouvet island", (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1FB'))
  , ("cocos (keeling) islands", (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1E8'))
  , ("christmas island", (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1FD'))
  , ("eritrea", (String.fromChar '\x1F1EA') ++ (String.fromChar '\x1F1F7'))
  , ("guernsey", (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1EC'))
  , ("gambia", (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1F2'))
  , ("guadeloupe", (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1F5'))
  , ("south georgia", (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1F8'))
  , ("guinea-bissau", (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1FC'))
  , ("heard island and mcdonald islands", (String.fromChar '\x1F1ED') ++ (String.fromChar '\x1F1F2'))
  , ("british indian ocean territory", (String.fromChar '\x1F1EE') ++ (String.fromChar '\x1F1F4'))
  , ("kiribati", (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1EE'))
  , ("comoros", (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1F2'))
  , ("saint kitts and nevis", (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1F3'))
  , ("northern mariana islands", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1F5'))
  , ("mauritania", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1F7'))
  , ("montserrat", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1F8'))
  , ("mauritius", (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1FA'))
  , ("new caledonia", (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1E8'))
  , ("norfolk island", (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1EB'))
  , ("nauru", (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1F7'))
  , ("niue", (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1FA'))
  , ("saint pierre and miquelon", (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1F2'))
  , ("pitcairn", (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1F3'))
  , ("palau", (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1FC'))
  , ("réunion", (String.fromChar '\x1F1F7') ++ (String.fromChar '\x1F1EA'))
  , ("svalbard and jan mayen", (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1EF'))
  , ("sint maarten (dutch part)", (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1FD'))
  , ("sao tome and principe", (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1F9'))
  , ("turks and caicos islands", (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1E8'))
  , ("tokelau", (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1F0'))
  , ("timor-leste", (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1F1'))
  , ("tuvalu", (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1FB'))
  , ("saint vincent and the grenadines", (String.fromChar '\x1F1FB') ++ (String.fromChar '\x1F1E8'))
  , ("vanuatu", (String.fromChar '\x1F1FB') ++ (String.fromChar '\x1F1FA'))
  , ("wallis and futuna", (String.fromChar '\x1F1FC') ++ (String.fromChar '\x1F1EB'))
  , ("mayotte", (String.fromChar '\x1F1FE') ++ (String.fromChar '\x1F1F9'))
  ]
