import Html.App as App

import Model exposing (init)
import Update exposing (update)
import View exposing (view)
import Subscriptions exposing (subscriptions)

main : Program Never
main = App.program
  { init = (init, Cmd.none)
  , update = update
  , view = view
  , subscriptions = subscriptions
  }
