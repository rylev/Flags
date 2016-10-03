import Time exposing (Time)
import Random
import String
import Array
import Dict exposing (Dict)
import Json.Decode exposing ((:=))
import Html exposing (Html, div, h1, text, p, input, form, button)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (style, placeholder, value)
import Html.App as App

type Event = Answer String | Noop | NewFlag String | Skip | Tick
type alias Model = { points: Int, currentFlag: String, currentInput: String, time: Time }

main : Program Never
main = App.program { init =  (init, generateNewFlag), update = update, view = view, subscriptions = subscription }

generateNewFlag : Cmd Event
generateNewFlag = Random.generate NewFlag newFlagGenerator

newFlagGenerator : Random.Generator String
newFlagGenerator = Random.map (atIndex flagMap germany) (randomIndex flagMap)

randomIndex : Dict comparable a -> Random.Generator Int
randomIndex map = Random.int 0 ((List.length (Dict.keys map)) - 1)

atIndex : Dict comparable a -> a -> Int -> a
atIndex dict default i = case dict |> Dict.toList |> Array.fromList |> Array.get i |> Maybe.map snd of
  Just item -> item
  Nothing -> default

update : Event -> Model -> (Model, Cmd Event)
update event model =
  case Debug.log "Event" event of
    Answer ans -> case Dict.get (String.toLower ans) flagMap of
      Just flag ->
        if flag == model.currentFlag then
          ({ model |  points = model.points + 1, currentInput = "" }, generateNewFlag)
        else
          ({ model | currentInput = ans }, Cmd.none)
      Nothing -> ({ model | currentInput = ans }, Cmd.none)
    NewFlag flag -> ({model | currentFlag = flag }, Cmd.none)
    Skip -> (model, generateNewFlag)
    Tick -> ({ model | time = model.time - tickRate }, Cmd.none)
    Noop -> (model, Cmd.none)

tickRate = 100 * Time.millisecond

view : Model -> Html Event
view model = div []
  [ title
  , points model.points
  , time model.time
  , flag model.currentFlag
  , answer model.currentInput
  , skipButton
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
answer currentValue = input [style [("font-size", "40px"), ("margin", "20px auto"), ("display", "block"), ("height", "50px"), ("width", "400px")], placeholder "Flag", value currentValue, onInput Answer ] []

skipButton : Html Event
skipButton = button [style [("font-size", "34px"), ("margin", "auto"), ("display", "block"), ("height", "50px"), ("width", "150px")], onClick Skip] [text "skip"]

subscription : Model -> Sub Event
subscription model = Time.every tickRate (\_ -> Tick)

init : Model
init = { points = 0, currentFlag = germany, currentInput = "", time = 20 * Time.second }

germany = (String.fromChar '\x1F1E9') ++ (String.fromChar '\x1F1EA')
uk = (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1E7')
usa = (String.fromChar '\x1F1FA') ++ (String.fromChar '\x1F1F8')

flagMap : Dict String String
flagMap =
  Dict.fromList
    [ ("china",  (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1F3'))
    , ("germany",  germany)
    , ("spain",  (String.fromChar '\x1F1EA') ++ (String.fromChar '\x1F1F8'))
    , ("france",  (String.fromChar '\x1F1EB') ++ (String.fromChar '\x1F1F7'))
    , ("uk",  uk)
    , ("great britain",  uk)
    , ("united kingdom",  uk)
    , ("italy",  (String.fromChar '\x1F1EE') ++ (String.fromChar '\x1F1F9'))
    , ("japan",  (String.fromChar '\x1F1EF') ++ (String.fromChar '\x1F1F5'))
    , ("south korea",  (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1F7'))
    , ("russia",  (String.fromChar '\x1F1F7') ++ (String.fromChar '\x1F1FA'))
    , ("united states",  usa)
    , ("us", usa)
    , ("andorra",  (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1E9'))
    -- ,("united arab emirates",  (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1EA'))
    -- ,("afghanistan",  (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1EB'))
    -- ,("antigua and barbuda",  (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1EC'))
    -- ,("anguilla",  (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1EE'))
    -- ,("albania",  (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1F1'))
    -- ,("armenia",  (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1F2'))
    -- ,("angola",  (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1F4'))
    -- ,("antarctica",  (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1F6'))
    -- ,("argentina",  (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1F7'))
    -- ,("american samoa",  (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1F8'))
    -- ,("austria",  (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1F9'))
    -- ,("australia",  (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1FA'))
    -- ,("aruba",  (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1FC'))
    -- ,("åland islands",  (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1FD'))
    -- ,("azerbaijan",  (String.fromChar '\x1F1E6') ++ (String.fromChar '\x1F1FF'))
    -- ,("bosnia and herzegovina",  (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1E6'))
    -- ,("barbados",  (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1E7'))
    -- ,("bangladesh",  (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1E9'))
    -- ,("belgium",  (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1EA'))
    -- ,("burkina faso",  (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1EB'))
    -- ,("bulgaria",  (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1EC'))
    -- ,("bahrain",  (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1ED'))
    -- ,("burundi",  (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1EE'))
    -- ,("benin",  (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1EF'))
    -- ,("saint barthélemy",  (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1F1'))
    -- ,("bermuda",  (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1F2'))
    -- ,("brunei darussalam",  (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1F3'))
    -- ,("bolivia",  (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1F4'))
    -- ,("bonaire, sint eustatius and saba",  (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1F6'))
    -- ,("brazil",  (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1F7'))
    -- ,("bahamas",  (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1F8'))
    -- ,("bhutan",  (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1F9'))
    -- ,("bouvet island",  (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1FB'))
    -- ,("botswana",  (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1FC'))
    -- ,("belarus",  (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1FE'))
    -- ,("belize",  (String.fromChar '\x1F1E7') ++ (String.fromChar '\x1F1FF'))
    -- ,("canada",  (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1E6'))
    -- ,("cocos (keeling) islands",  (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1E8'))
    -- ,("congo",  (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1E9'))
    -- ,("central african republic",  (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1EB'))
    -- ,("congo",  (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1EC'))
    -- ,("switzerland",  (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1ED'))
    -- ,("côte d'ivoire",  (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1EE'))
    -- ,("cook islands",  (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1F0'))
    -- ,("chile",  (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1F1'))
    -- ,("cameroon",  (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1F2'))
    -- ,("china",  (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1F3'))
    -- ,("colombia",  (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1F4'))
    -- ,("costa rica",  (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1F7'))
    -- ,("cuba",  (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1FA'))
    -- ,("cape verde",  (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1FB'))
    -- ,("curaçao",  (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1FC'))
    -- ,("christmas island",  (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1FD'))
    -- ,("cyprus",  (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1FE'))
    -- ,("czech republic",  (String.fromChar '\x1F1E8') ++ (String.fromChar '\x1F1FF'))
    -- ,("germany",  (String.fromChar '\x1F1E9') ++ (String.fromChar '\x1F1EA'))
    -- ,("djibouti",  (String.fromChar '\x1F1E9') ++ (String.fromChar '\x1F1EF'))
    -- ,("denmark",  (String.fromChar '\x1F1E9') ++ (String.fromChar '\x1F1F0'))
    -- ,("dominica",  (String.fromChar '\x1F1E9') ++ (String.fromChar '\x1F1F2'))
    -- ,("dominican republic",  (String.fromChar '\x1F1E9') ++ (String.fromChar '\x1F1F4'))
    -- ,("algeria",  (String.fromChar '\x1F1E9') ++ (String.fromChar '\x1F1FF'))
    -- ,("ecuador",  (String.fromChar '\x1F1EA') ++ (String.fromChar '\x1F1E8'))
    -- ,("estonia",  (String.fromChar '\x1F1EA') ++ (String.fromChar '\x1F1EA'))
    -- ,("egypt",  (String.fromChar '\x1F1EA') ++ (String.fromChar '\x1F1EC'))
    -- ,("western sahara",  (String.fromChar '\x1F1EA') ++ (String.fromChar '\x1F1ED'))
    -- ,("eritrea",  (String.fromChar '\x1F1EA') ++ (String.fromChar '\x1F1F7'))
    -- ,("spain",  (String.fromChar '\x1F1EA') ++ (String.fromChar '\x1F1F8'))
    -- ,("ethiopia",  (String.fromChar '\x1F1EA') ++ (String.fromChar '\x1F1F9'))
    -- ,("finland",  (String.fromChar '\x1F1EB') ++ (String.fromChar '\x1F1EE'))
    -- ,("fiji",  (String.fromChar '\x1F1EB') ++ (String.fromChar '\x1F1EF'))
    -- ,("falkland islands (malvinas)",  (String.fromChar '\x1F1EB') ++ (String.fromChar '\x1F1F0'))
    -- ,("micronesia",  (String.fromChar '\x1F1EB') ++ (String.fromChar '\x1F1F2'))
    -- ,("faroe islands",  (String.fromChar '\x1F1EB') ++ (String.fromChar '\x1F1F4'))
    -- ,("france",  (String.fromChar '\x1F1EB') ++ (String.fromChar '\x1F1F7'))
    -- ,("gabon",  (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1E6'))
    -- ,("united kingdom",  (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1E7'))
    -- ,("grenada",  (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1E9'))
    -- ,("georgia",  (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1EA'))
    -- ,("french guiana",  (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1EB'))
    -- ,("guernsey",  (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1EC'))
    -- ,("ghana",  (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1ED'))
    -- ,("gibraltar",  (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1EE'))
    -- ,("greenland",  (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1F1'))
    -- ,("gambia",  (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1F2'))
    -- ,("guinea",  (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1F3'))
    -- ,("guadeloupe",  (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1F5'))
    -- ,("equatorial guinea",  (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1F6'))
    -- ,("greece",  (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1F7'))
    -- ,("south georgia",  (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1F8'))
    -- ,("guatemala",  (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1F9'))
    -- ,("guam",  (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1FA'))
    -- ,("guinea-bissau",  (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1FC'))
    -- ,("guyana",  (String.fromChar '\x1F1EC') ++ (String.fromChar '\x1F1FE'))
    -- ,("hong kong",  (String.fromChar '\x1F1ED') ++ (String.fromChar '\x1F1F0'))
    -- ,("heard island and mcdonald islands",  (String.fromChar '\x1F1ED') ++ (String.fromChar '\x1F1F2'))
    -- ,("honduras",  (String.fromChar '\x1F1ED') ++ (String.fromChar '\x1F1F3'))
    -- ,("croatia",  (String.fromChar '\x1F1ED') ++ (String.fromChar '\x1F1F7'))
    -- ,("haiti",  (String.fromChar '\x1F1ED') ++ (String.fromChar '\x1F1F9'))
    -- ,("hungary",  (String.fromChar '\x1F1ED') ++ (String.fromChar '\x1F1FA'))
    -- ,("indonesia",  (String.fromChar '\x1F1EE') ++ (String.fromChar '\x1F1E9'))
    -- ,("ireland",  (String.fromChar '\x1F1EE') ++ (String.fromChar '\x1F1EA'))
    -- ,("israel",  (String.fromChar '\x1F1EE') ++ (String.fromChar '\x1F1F1'))
    -- ,("isle of man",  (String.fromChar '\x1F1EE') ++ (String.fromChar '\x1F1F2'))
    -- ,("india",  (String.fromChar '\x1F1EE') ++ (String.fromChar '\x1F1F3'))
    -- ,("british indian ocean territory",  (String.fromChar '\x1F1EE') ++ (String.fromChar '\x1F1F4'))
    -- ,("iraq",  (String.fromChar '\x1F1EE') ++ (String.fromChar '\x1F1F6'))
    -- ,("iran",  (String.fromChar '\x1F1EE') ++ (String.fromChar '\x1F1F7'))
    -- ,("iceland",  (String.fromChar '\x1F1EE') ++ (String.fromChar '\x1F1F8'))
    -- ,("italy",  (String.fromChar '\x1F1EE') ++ (String.fromChar '\x1F1F9'))
    -- ,("jersey",  (String.fromChar '\x1F1EF') ++ (String.fromChar '\x1F1EA'))
    -- ,("jamaica",  (String.fromChar '\x1F1EF') ++ (String.fromChar '\x1F1F2'))
    -- ,("jordan",  (String.fromChar '\x1F1EF') ++ (String.fromChar '\x1F1F4'))
    -- ,("japan",  (String.fromChar '\x1F1EF') ++ (String.fromChar '\x1F1F5'))
    -- ,("kenya",  (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1EA'))
    -- ,("kyrgyzstan",  (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1EC'))
    -- ,("cambodia",  (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1ED'))
    -- ,("kiribati",  (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1EE'))
    -- ,("comoros",  (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1F2'))
    -- ,("saint kitts and nevis",  (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1F3'))
    -- ,("north korea",  (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1F5'))
    -- ,("south korea",  (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1F7'))
    -- ,("kuwait",  (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1FC'))
    -- ,("cayman islands",  (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1FE'))
    -- ,("kazakhstan",  (String.fromChar '\x1F1F0') ++ (String.fromChar '\x1F1FF'))
    -- ,("lao people's democratic republic",  (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1E6'))
    -- ,("lebanon",  (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1E7'))
    -- ,("saint lucia",  (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1E8'))
    -- ,("liechtenstein",  (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1EE'))
    -- ,("sri lanka",  (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1F0'))
    -- ,("liberia",  (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1F7'))
    -- ,("lesotho",  (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1F8'))
    -- ,("lithuania",  (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1F9'))
    -- ,("luxembourg",  (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1FA'))
    -- ,("latvia",  (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1FB'))
    -- ,("libya",  (String.fromChar '\x1F1F1') ++ (String.fromChar '\x1F1FE'))
    -- ,("morocco",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1E6'))
    -- ,("monaco",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1E8'))
    -- ,("moldova",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1E9'))
    -- ,("montenegro",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1EA'))
    -- ,("saint martin (french part)",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1EB'))
    -- ,("madagascar",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1EC'))
    -- ,("marshall islands",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1ED'))
    -- ,("macedonia",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1F0'))
    -- ,("mali",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1F1'))
    -- ,("myanmar",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1F2'))
    -- ,("mongolia",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1F3'))
    -- ,("macao",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1F4'))
    -- ,("northern mariana islands",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1F5'))
    -- ,("martinique",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1F6'))
    -- ,("mauritania",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1F7'))
    -- ,("montserrat",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1F8'))
    -- ,("malta",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1F9'))
    -- ,("mauritius",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1FA'))
    -- ,("maldives",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1FB'))
    -- ,("malawi",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1FC'))
    -- ,("mexico",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1FD'))
    -- ,("malaysia",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1FE'))
    -- ,("mozambique",  (String.fromChar '\x1F1F2') ++ (String.fromChar '\x1F1FF'))
    -- ,("namibia",  (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1E6'))
    -- ,("new caledonia",  (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1E8'))
    -- ,("niger",  (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1EA'))
    -- ,("norfolk island",  (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1EB'))
    -- ,("nigeria",  (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1EC'))
    -- ,("nicaragua",  (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1EE'))
    -- ,("netherlands",  (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1F1'))
    -- ,("norway",  (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1F4'))
    -- ,("nepal",  (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1F5'))
    -- ,("nauru",  (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1F7'))
    -- ,("niue",  (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1FA'))
    -- ,("new zealand",  (String.fromChar '\x1F1F3') ++ (String.fromChar '\x1F1FF'))
    -- ,("oman",  (String.fromChar '\x1F1F4') ++ (String.fromChar '\x1F1F2'))
    -- ,("panama",  (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1E6'))
    -- ,("peru",  (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1EA'))
    -- ,("french polynesia",  (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1EB'))
    -- ,("papua new guinea",  (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1EC'))
    -- ,("philippines",  (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1ED'))
    -- ,("pakistan",  (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1F0'))
    -- ,("poland",  (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1F1'))
    -- ,("saint pierre and miquelon",  (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1F2'))
    -- ,("pitcairn",  (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1F3'))
    -- ,("puerto rico",  (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1F7'))
    -- ,("palestinian territory",  (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1F8'))
    -- ,("portugal",  (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1F9'))
    -- ,("palau",  (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1FC'))
    -- ,("paraguay",  (String.fromChar '\x1F1F5') ++ (String.fromChar '\x1F1FE'))
    -- ,("qatar",  (String.fromChar '\x1F1F6') ++ (String.fromChar '\x1F1E6'))
    -- ,("réunion",  (String.fromChar '\x1F1F7') ++ (String.fromChar '\x1F1EA'))
    -- ,("romania",  (String.fromChar '\x1F1F7') ++ (String.fromChar '\x1F1F4'))
    -- ,("serbia",  (String.fromChar '\x1F1F7') ++ (String.fromChar '\x1F1F8'))
    -- ,("russia",  (String.fromChar '\x1F1F7') ++ (String.fromChar '\x1F1FA'))
    -- ,("rwanda",  (String.fromChar '\x1F1F7') ++ (String.fromChar '\x1F1FC'))
    -- ,("saudi arabia",  (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1E6'))
    -- ,("solomon islands",  (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1E7'))
    -- ,("seychelles",  (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1E8'))
    -- ,("sudan",  (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1E9'))
    -- ,("sweden",  (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1EA'))
    -- ,("singapore",  (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1EC'))
    -- ,("saint helena, ascension and tristan da cunha",  (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1ED'))
    -- ,("slovenia",  (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1EE'))
    -- ,("svalbard and jan mayen",  (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1EF'))
    -- ,("slovakia",  (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1F0'))
    -- ,("sierra leone",  (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1F1'))
    -- ,("san marino",  (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1F2'))
    -- ,("senegal",  (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1F3'))
    -- ,("somalia",  (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1F4'))
    -- ,("suriname",  (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1F7'))
    -- ,("south sudan",  (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1F8'))
    -- ,("sao tome and principe",  (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1F9'))
    -- ,("el salvador",  (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1FB'))
    -- ,("sint maarten (dutch part)",  (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1FD'))
    -- ,("syrian arab republic",  (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1FE'))
    -- ,("swaziland",  (String.fromChar '\x1F1F8') ++ (String.fromChar '\x1F1FF'))
    -- ,("turks and caicos islands",  (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1E8'))
    -- ,("chad",  (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1E9'))
    -- ,("french southern territories",  (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1EB'))
    -- ,("togo",  (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1EC'))
    -- ,("thailand",  (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1ED'))
    -- ,("tajikistan",  (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1EF'))
    -- ,("tokelau",  (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1F0'))
    -- ,("timor-leste",  (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1F1'))
    -- ,("turkmenistan",  (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1F2'))
    -- ,("tunisia",  (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1F3'))
    -- ,("tonga",  (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1F4'))
    -- ,("turkey",  (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1F7'))
    -- ,("trinidad and tobago",  (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1F9'))
    -- ,("tuvalu",  (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1FB'))
    -- ,("taiwan",  (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1FC'))
    -- ,("tanzania",  (String.fromChar '\x1F1F9') ++ (String.fromChar '\x1F1FF'))
    -- ,("ukraine",  (String.fromChar '\x1F1FA') ++ (String.fromChar '\x1F1E6'))
    -- ,("uganda",  (String.fromChar '\x1F1FA') ++ (String.fromChar '\x1F1EC'))
    -- ,("united states minor outlying islands",  (String.fromChar '\x1F1FA') ++ (String.fromChar '\x1F1F2'))
    -- ,("united states",  (String.fromChar '\x1F1FA') ++ (String.fromChar '\x1F1F8'))
    -- ,("uruguay",  (String.fromChar '\x1F1FA') ++ (String.fromChar '\x1F1FE'))
    -- ,("uzbekistan",  (String.fromChar '\x1F1FA') ++ (String.fromChar '\x1F1FF'))
    -- ,("vatican city",  (String.fromChar '\x1F1FB') ++ (String.fromChar '\x1F1E6'))
    -- ,("saint vincent and the grenadines",  (String.fromChar '\x1F1FB') ++ (String.fromChar '\x1F1E8'))
    -- ,("venezuela",  (String.fromChar '\x1F1FB') ++ (String.fromChar '\x1F1EA'))
    -- ,("virgin islands, british",  (String.fromChar '\x1F1FB') ++ (String.fromChar '\x1F1EC'))
    -- ,("virgin islands, u.s.",  (String.fromChar '\x1F1FB') ++ (String.fromChar '\x1F1EE'))
    -- ,("viet nam",  (String.fromChar '\x1F1FB') ++ (String.fromChar '\x1F1F3'))
    -- ,("vanuatu",  (String.fromChar '\x1F1FB') ++ (String.fromChar '\x1F1FA'))
    -- ,("wallis and futuna",  (String.fromChar '\x1F1FC') ++ (String.fromChar '\x1F1EB'))
    -- ,("samoa",  (String.fromChar '\x1F1FC') ++ (String.fromChar '\x1F1F8'))
    -- ,("yemen",  (String.fromChar '\x1F1FE') ++ (String.fromChar '\x1F1EA'))
    -- ,("mayotte",  (String.fromChar '\x1F1FE') ++ (String.fromChar '\x1F1F9'))
    -- ,("south africa",  (String.fromChar '\x1F1FF') ++ (String.fromChar '\x1F1E6'))
    -- ,("zambia",  (String.fromChar '\x1F1FF') ++ (String.fromChar '\x1F1F2'))
    -- ,("zimbabwe",  (String.fromChar '\x1F1FF') ++ (String.fromChar '\x1F1FC'))
    ]
