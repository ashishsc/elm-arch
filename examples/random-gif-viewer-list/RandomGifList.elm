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
          -- most of these are Effects.none
        , batch fxList
        )



-- VIEW
-- => is frustrating, but the tutorial included it


(=>) : a -> b -> ( a, b )
(=>) =
  (,)


view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [ input
        [ placeholder "What kind of gifs do you want"
        , value model.topic
        , onEnter address Create
          {- on : String -> Decoder String(a) -> (String(a) -> Message) -> Attribute
          Topic : String -> Action
          Signal.message : Address Action(a) -> Action(a) -> Message
           so (Signal.message address << Topic) becomes  String -> Message
          on's 3rd arg needs to be String(a) -> Message because
          targetValue is Decoder String(a)
          in plain terms,
          on needs a way to send the decoded string to the address
          The function we define here converts the string to an action
          and creates a message for the address with that Action tagged string
          -}
        , on "input" targetValue (Signal.message address << Topic)
        , inputStyle
        ]
        []
    , div
        [ style [ "display" => "flex", "flex-wrap" => "wrap" ] ]
        (List.map (elementView address) model.gifList)
    ]


elementView : Signal.Address Action -> ( Int, RandomGif.Model ) -> Html
elementView address ( id, model ) =
  RandomGif.view (Signal.forwardTo address (SubMsg id)) model


inputStyle : Attribute
inputStyle =
  style
    [ ( "width", "100%" )
    , ( "height", "40px" )
    , ( "padding", "10px 0" )
    , ( "font-size", "2em" )
    , ( "text-align", "center" )
    ]


onEnter : Signal.Address a -> a -> Attribute
onEnter address value =
  let
    is13 code =
      if code == 13 then
        Ok ()
      else
        Err "not the right key code"
  in
    on
      "keydown"
      -- Decoder ()
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address value)
