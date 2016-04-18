module CounterListFancy (init, update, view) where

import Counter
import Html exposing (..)
import Html.Events exposing (..)


-- Model


type alias Model =
  { counters : List ( ID, Counter.Model )
  , nextID : ID
  }


type alias ID =
  Int


init : Model
init =
  { counters = []
  , nextID = 0
  }



-- Update


type Action
  = Insert
  | Remove ID
  | Modify ID Counter.Action


update : Action -> Model -> Model
update action model =
  case action of
    Insert ->
      { model
        | counters = ( model.nextID, Counter.init 0 ) :: model.counters
        , nextID = model.nextID + 1
      }

    Remove id ->
      { model
        | -- only keep counters that don't have this id
          counters = List.filter (\( counterId, _ ) -> counterId /= id) model.counters
      }

    Modify id counterAction ->
      let
        updateCounter ( counterId, counterModel ) =
          if counterId == id then
            ( counterId, Counter.update counterAction counterModel )
          else
            ( counterId, counterModel )
      in
        { model | counters = List.map updateCounter model.counters }


view : Signal.Address Action -> Model -> Html
view address model =
  let
    insert =
      button [ onClick address Insert ] [ text "Add" ]
  in
    div [] (insert :: List.map (viewCounter address) model.counters)


viewCounter : Signal.Address Action -> ( ID, Counter.Model ) -> Html
viewCounter address ( id, model ) =
  let
    context =
      Counter.Context
        (Signal.forwardTo address (Modify id))
        (Signal.forwardTo address (always (Remove id)))
  in
    Counter.viewWithRemoveButton context model
