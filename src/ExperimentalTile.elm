module ExperimentalTile exposing (Model, Msg, updateTick, init, update, view)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


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
  -- { model | diff = newDiff }

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

view : Model -> Html Msg
view model =
  div [ class "experimentalTile"
      , class (if model.on then "experimentalOn" else "experimentalOff")
      , onClick Rotate
      ] [ text (toString model.id) ]
