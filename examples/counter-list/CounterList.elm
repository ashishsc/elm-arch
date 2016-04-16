module CounterList (..) where

import Counter
import Html exposing (..)
import Html.Events exposing (..)


-- Model


type alias ID =
  Int


type alias Model =
  { counters : List ( ID, Counter.Model )
  , nextID : ID
  }


init : Model
init =
  { counters = []
  , nextID = 0
  }



-- Update


type Action
  = Insert
  | Remove
  | Modify ID Counter.Action


update : Action -> Model -> Model
update action model =
  case action of
    Insert ->
      let
        newCounter =
          ( model.nextID, Counter.init 0 )

        newCounters =
          model.counters ++ [ newCounter ]

        nextID' =
          model.nextID + 1
      in
        { model
          | counters = newCounters
          , nextID = nextID'
        }

    Remove ->
      let
        len =
          List.length model.counters
      in
        { model | counters = List.take (len - 1) model.counters }

    Modify id counterAction ->
      let
        updateCounter ( counterId, counterModel ) =
          if counterId == id then
            ( counterId, Counter.update counterAction counterModel )
          else
            ( counterId, counterModel )
      in
        { model | counters = List.map updateCounter model.counters }



-- View


viewCounter : Signal.Address Action -> ( ID, Counter.Model ) -> Html
viewCounter address ( id, model ) =
  {- Create a new address to give to the view of Counter
  That only alows Modify actions to be sent to CounterList's
  update function
  The signal will always output a Modify with the id we passed in
  on the given model
  We bind in the id so that the modifications always go to a specific counter
  -}
  let
    modifyAddress =
      Signal.forwardTo address (Modify id)
  in
    Counter.view modifyAddress model


view : Signal.Address Action -> Model -> Html
view address model =
  let
    counters =
      -- Partial function application to bind
      -- the address to viewCounter
      List.map (viewCounter address) model.counters

    remove =
      button [ onClick address Remove ] [ text "Remove" ]

    insert =
      button [ onClick address Insert ] [ text "Add a new counter" ]
  in
    div [] ([ remove, insert ] ++ counters)
