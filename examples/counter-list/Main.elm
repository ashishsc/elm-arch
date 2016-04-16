module Main (..) where

import CounterList exposing (update, view)
import StartApp.Simple exposing (start)


main =
  start
    { model = CounterList.init
    , update = update
    , view = view
    }
