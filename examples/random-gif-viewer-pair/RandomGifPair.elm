module RandomGifPair (..) where

import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (style)
import RandomGif


-- MODEL


type alias Model =
  { left : RandomGif.Model
  , right : RandomGif.Model
  }


init : String -> String -> ( Model, Effects Action )
init leftTopic rightTopic =
  let
    ( left, leftFx ) =
      RandomGif.init leftTopic

    ( right, rightFx ) =
      RandomGif.init rightTopic
  in
    ( Model left right
      -- like promise#all
    , Effects.batch [ Effects.map Left leftFx, Effects.map Right rightFx ]
    )



-- UPDATE


type Action
  = Left RandomGif.Action
  | Right RandomGif.Action


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Left msg ->
      let
        ( left, fx ) =
          RandomGif.update msg model.left
      in
        ( Model left model.right
        , Effects.map Left fx
          -- Tag the effect with the Left type
        )

    Right msg ->
      let
        ( right, fx ) =
          RandomGif.update msg model.right
      in
        ( Model model.left right
        , Effects.map Right fx
          -- Tag the effect with the Right type
        )



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ style [ ( "display", "flex" ) ] ]
    [ RandomGif.view (Signal.forwardTo address Left) model.left
    , RandomGif.view (Signal.forwardTo address Right) model.right
    ]
