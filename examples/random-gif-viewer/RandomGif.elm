module RandomGif (init, update, view) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Http
import Json.Decode as Json
import Task


-- This is stupid, but the tutorial included it


(=>) : a -> b -> ( a, b )
(=>) =
  (,)



-- MODEL
-- TODO maybe add the ability for the user to specify which topic they want


type alias Model =
  { topic : String
  , gifUrl : String
  }


init : String -> ( Model, Effects Action )
init topic =
  ( Model topic "assets/waiting.gif"
  , getRandomGif topic
  )



-- EFFECTS


getRandomGif : String -> Effects Action
getRandomGif topic =
  -- get the json
  Http.get decodeImageUrl (randomUrl topic)
    |> -- map the error to a maybe
       Task.toMaybe
    |> -- Tag the result with the NewGif Action
       Task.map NewGif
    |> --  convert a task into an effect to be used in update/view
       Effects.task



-- Construct an appropriately encoded url for the giphy api
-- i.e http://api.giphy.com/v1/gifs/random?api_key=dc7&tag=cats


randomUrl : String -> String
randomUrl topic =
  Http.url
    "http://api.giphy.com/v1/gifs/random"
    -- List (String, String)
    [ "api_key" => "dc6zaTOxFJmzC"
    , "tag" => topic
    ]



-- From the Http response, grab data.image_url out of the json
-- extract the type as a String


decodeImageUrl : Json.Decoder String
decodeImageUrl =
  Json.at [ "data", "image_url" ] Json.string



-- UPDATE


type Action
  = RequestMore
  | NewGif (Maybe String)
  | NewTopic String


update : Action -> Model -> ( Model, Effects Action )
update msg model =
  case msg of
    RequestMore ->
      ( model, getRandomGif model.topic )

    NewGif maybeUrl ->
      ( Model model.topic (Maybe.withDefault model.gifUrl maybeUrl)
      , Effects.none
      )

    NewTopic topic' ->
      ( { model | topic = topic' }
      , Effects.none
      )



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  let
    stringAddress =
      Signal.forwardTo address NewTopic
  in
    div
      [ style [ "width" => "200px" ] ]
      [ h2 [ headerStyle ] [ text model.topic ]
      , div [ imgStyle model.gifUrl ] []
      , button [ onClick address RequestMore ] [ text "More Please!" ]
      , input
          [ placeholder "new topic"
          , value model.topic
          , on "input" targetValue (Signal.message stringAddress)
          ]
          []
      ]



-- View styles


headerStyle : Attribute
headerStyle =
  style
    [ "width" => "200px", "text-align" => "center" ]


imgStyle : String -> Attribute
imgStyle url =
  style
    [ "display" => "inline-block"
    , "width" => "200px"
    , "height" => "200px"
    , "background-position" => "center center"
    , "background-size" => "cover"
    , "background-image" => ("url('" ++ url ++ "')")
    ]
