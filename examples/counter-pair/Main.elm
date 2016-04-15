module Main (..) where

import CounterPair exposing (update, view)
import StartApp.Simple exposing (start)


main =
  start
    { model = CounterPair.init 0 0
    , update = update
    , view = view
    }
