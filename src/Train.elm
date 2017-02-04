module Train exposing (Model, Progress(..), init, view, updateProgress)
import Tile exposing (Model, Direction)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Css exposing (asPairs, px, opacity, left, top)


-- MODEL

type Progress = ProgressingSince Float | Done

type alias Model =
  { index: Int
  , from: Tile.Direction
  , to: Tile.Direction
  , progress: Progress
  }

init : Int -> Tile.Model -> Model
init index tile =
  let
    (from, to) = Tile.connections tile
  in
    Model index from to (ProgressingSince 0)

updateProgress : Float -> Model -> Model
updateProgress diff model =
  let
    progress = case model.progress of
      Done -> ProgressingSince diff
      ProgressingSince time ->
        if time < progressTime
        then ProgressingSince (time + diff)
        else Done
  in
    { model | progress = progress }

progressTime : Float
progressTime = 2000


-- VIEW

calculateOpacity : Model -> Css.Mixin
calculateOpacity { progress } =
  let
    value = case progress of
      ProgressingSince time -> 1 - 1 / 1000 * time
      Done -> 1
  in
    opacity (Css.num value)

calculateOffsets : (Int, Int) -> List Css.Mixin
calculateOffsets coordinates =
  let
    x = toFloat (Tuple.first coordinates) * 80
    y = toFloat (Tuple.second coordinates) * 80
  in
    [top (px y), left (px x)]

styles : List Css.Mixin -> Html.Attribute msg
styles =
  Css.asPairs >> Html.Attributes.style

view : (Int -> List Css.Mixin) -> Model -> Html msg
view calculateOffsets model =
  div [ class "train"
      , styles (calculateOpacity model :: calculateOffsets model.index)
      ] []
