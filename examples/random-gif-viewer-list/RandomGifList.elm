module RandomGifList (init, update, view) where

import Effects exposing (Effects, map, batch, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import RandomGif


-- MODEL


type alias Model =
  { topic : String
  , gifList : List ( Int, RandomGif.Model )
  , uid : Int
  }


init : ( Model, Effects Action )
init =
  ( Model "" [] 0
  , Effects.none
  )



-- UPDATE


type Action
  = Topic String
  | Create
  | SubMsg Int RandomGif.Action


update : Action -> Model -> ( Model, Effects Action )
update message model =
  case message of
    Topic topic ->
      ( { model | topic = topic }
      , Effects.none
      )

    Create ->
      -- Clear out the topic and add to the list
      let
        ( newRandomGif, fx ) =
          RandomGif.init model.topic

        newModel =
          Model "" (model.gifList ++ [ ( model.uid, newRandomGif ) ]) (model.uid + 1)
      in
        ( newModel, map (SubMsg model.uid) fx )

    -- tag the RGAction as a RGLAction
    SubMsg msgId msg ->
      let
        -- fn to update specific RG and get its effect
        subUpdate (( id, randomGif ) as entry) =
          if id == msgId then
            let
              ( newRandomGif, fx ) =
                RandomGif.update msg randomGif
            in
              ( ( id, newRandomGif )
              , map (SubMsg id) fx
              )
          else
            ( entry, Effects.none )

        -- Update the RG that needs updating and split up the lists
        ( newGifList, fxList ) =
          model.gifList
            |> List.map subUpdate
            |> List.unzip
      in
        ( { model | gifList = newGifList }
        , batch fxList
        )
