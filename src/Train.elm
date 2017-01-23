module Train exposing (Model, init, view, updateTick)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Css exposing (asPairs, opacity)


-- MODEL

type alias TileCoordinates = (Int, Int)

type alias Model =
  { coordinates: TileCoordinates
  , progress: Float
  }

init : TileCoordinates -> Model
init coordinates =
  Model coordinates 0

updateTick : Float -> Model -> Model
updateTick diff model =
  { model | progress =
    if model.progress < progressTime
    then (model.progress + diff)
    else 0
  }

progressTime : Float
progressTime = 1000

-- VIEW

calculateOpacity : Model -> Css.Mixin
calculateOpacity { progress } =
  let
    value = 1 - 1 / 1000 * progress
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
