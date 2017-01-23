module Train exposing (Model, init, view, updateTick)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Css exposing (asPairs, opacity)


-- MODEL

type Progress = Done | ProgressingSince Float

type alias Model =
  { tileIndex: Int
  , progress: Progress
  }

init : Int -> Model
init tileIndex = Model tileIndex (ProgressingSince 0)

updateTick : Float -> Model -> Model
updateTick diff model =
  case model.progress of
    Done -> model
    ProgressingSince time ->
      { model | progress =
        if time < progressTime
        then ProgressingSince (time + diff)
        else Done
      }

progressTime : Float
progressTime = 1000


-- VIEW

calculateOpacity : Model -> Css.Mixin
calculateOpacity { progress } =
  let
    value = case progress of
      Done -> 0
      ProgressingSince time -> 1 - 1 / 1000 * time
  in
    opacity (Css.num value)

styles : List Css.Mixin -> Html.Attribute msg
styles =
  Css.asPairs >> Html.Attributes.style

view : Model -> Html msg
view model =
  div [ class "train"
      , styles [ calculateOpacity model ]
      ] []
