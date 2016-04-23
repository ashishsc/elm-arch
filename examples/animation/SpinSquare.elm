module SpinSquare (Model, Action, init, update, view) where

import Easing exposing (ease, easeOutBounce, float)
import Effects exposing (Effects)
import Html exposing (Html)
import Svg exposing (svg, rect, g, text, text')
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Time exposing (Time, second)


-- MODEL


type alias Model =
  { angle : Float
  , animationState : AnimationState
  }


type alias AnimationState =
  Maybe { prevClockTime : Time, elapsedTime : Time }


init : ( Model, Effects Action )
init =
  ( { angle = 0, animationState = Nothing }
  , Effects.none
  )


rotateStep : Float
rotateStep =
  90


duration : Time
duration =
  second



-- UPDATE


type Action
  = Spin
  | Tick Time


update : Action -> Model -> ( Model, Effects Action )
update msg model =
  case msg of
    Spin ->
      case model.animationState of
        Nothing ->
          -- if we're not animating, start one
          ( model, Effects.tick Tick )

        Just _ ->
          -- if we are animating, ignore the update
          ( model, Effects.none )

    Tick clockTime ->
      let
        newElapsedTime =
          case model.animationState of
            Nothing ->
              0

            Just { elapsedTime, prevClockTime } ->
              elapsedTime + (clockTime - prevClockTime)
      in
        -- Animation should be over now, set the state to none and
        -- the angle to its final resting point
        if newElapsedTime > duration then
          ( { angle = model.angle + rotateStep
            , animationState = Nothing
            }
          , Effects.none
          )
        else
          -- still animation left, trigger another tick, update elapsedTime
          ( { angle = model.angle
            , animationState = Just { elapsedTime = newElapsedTime, prevClockTime = clockTime }
            }
          , Effects.tick Tick
          )


view : Signal.Address Action -> Model -> Html
view address model =
  Html.div [] []
