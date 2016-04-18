module Main (..) where

import CounterListFancy exposing (update, view)
import StartApp.Simple exposing (start)


main =
  start
    { model = CounterListFancy.init
    , update = update
    , view = view
    }
