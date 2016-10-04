module Model exposing (..)

import Data exposing (..)
import Time exposing (Time)

type Model =
  StartMenu StartMenuState
  | ActiveGame ActiveGameState
  | GameOverMenu GameOverState

type alias StartMenuState = { difficultyLevel: DifficultyLevel }
type alias ActiveGameState =
  { points: Int
  , flagInfo: FlagInfo
  , currentInput: String
  , time: Time
  , difficultyLevel: DifficultyLevel
  , lastWrongQuestion: Maybe String
  }
type alias GameOverState = { points: Int }

init : Model
init = StartMenu { difficultyLevel = Level1 }

newGame : DifficultyLevel -> Model
newGame difficultyLevel = ActiveGame
  { difficultyLevel = difficultyLevel
  , points = 0
  , flagInfo = { flag = germany, countryName = "Germany" }
  , time = 15 * Time.second
  , currentInput = ""
  , lastWrongQuestion = Nothing
  }


type Event =
  NewInput String
  | Submit
  | NewFlag FlagInfo
  | Skip String
  | Tick Time
  | Start
  | ChangeDifficulty DifficultyLevel
  | RemoveWrongAnswer

type DifficultyLevel = Level1 | Level2 | Level3 | Level4 | Level5
type alias FlagInfo = { countryName: String, flag: String }
