module Model exposing
  ( init
  , newGame
  , generateNewFlag
  , flagDatabase
  , timePerQuestion
  , Model(..)
  , Event(..)
  , DifficultyLevel(..)
  , StartMenuState
  , ActiveGameState
  , GameOverState
  )

import Dict exposing (Dict)
import Array
import Time exposing (Time)
import Random

import Data exposing (..)

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
  , time = timePerQuestion
  , currentInput = ""
  , lastWrongQuestion = Nothing
  }

timePerQuestion = 10 * Time.second


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
