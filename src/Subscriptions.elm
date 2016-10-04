module Subscriptions exposing (..)

import Time exposing (Time)
import Model exposing (..)


subscriptions : Model -> Sub Event
subscriptions model = Time.every tickRate (\_ -> Tick tickRate)

tickRate : Time
tickRate = 100 * Time.millisecond
