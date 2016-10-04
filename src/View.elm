module View exposing (..)

import Time exposing (Time)
import String
import Json.Decode exposing ((:=))
import Html exposing (Html, div, span, h1, text, p, input, form, button, fieldset, label)
import Html.Events exposing (on, onInput, onClick)
import Html.Attributes exposing (style, placeholder, value, type', name, checked)

import Model exposing (..)

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

