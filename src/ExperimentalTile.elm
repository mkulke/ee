module ExperimentalTile exposing (Model, Msg, updateTick, init, update, view)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Css exposing (asPairs, transform, deg, rotate)


-- MODEL

type alias Model =
  { id: Int
  , on: Bool
  , transitioning: Transition
  }

type Transition = NotTransitioning | TransitioningSince Float

init : Int -> Model
init id =
  Model id False NotTransitioning

updateTick : Float -> Model -> Model
updateTick diff model =
  case model.transitioning of
    NotTransitioning -> model
    TransitioningSince time ->
      { model | transitioning =
        if time < 1000
        then TransitioningSince (time + diff)
        else NotTransitioning
      }


-- MSG

type Msg = Rotate


-- UPDATE

update : Msg -> Model -> Model
update msg model =
  case msg of
    Rotate ->
      if model.transitioning == NotTransitioning
      then { model | on = xor model.on True, transitioning = TransitioningSince 0 }
      else model


-- VIEW

styles : List Css.Mixin -> Html.Attribute msg
styles =
  Css.asPairs >> Html.Attributes.style

rotation transition =
  case transition of
    NotTransitioning -> deg 0
    TransitioningSince time -> deg (360 / 1000 * time)

view : Model -> Html Msg
view model =
  div [ class "experimentalTile"
      , styles [ transform (rotate (rotation model.transitioning)) ]
      , class (if model.on then "experimentalOn" else "experimentalOff")
      , onClick Rotate
      ] [ text (toString model.id) ]
