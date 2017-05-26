module Train exposing (Model, Progress(..), init, view, updateProgress)

import Tile exposing (Model, Direction)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Css exposing (asPairs, px, opacity, left, top)


-- MODEL


type Progress
    = ProgressingSince Float
    | Done


type alias Model =
    { index : Int
    , from : Tile.Direction
    , to : Tile.Direction
    , progress : Progress
    }


init : Int -> Tile.Model -> Model
init index tile =
    let
        ( from, to ) =
            Tile.connections tile
    in
        Model index from to (ProgressingSince 0)


updateProgress : Float -> Model -> Model
updateProgress diff model =
    let
        progress =
            case model.progress of
                Done ->
                    ProgressingSince diff

                ProgressingSince time ->
                    if time < progressTime then
                        ProgressingSince (time + diff)
                    else
                        Done
    in
        { model | progress = progress }


progressTime : Float
progressTime =
    2000


progressFactor : Progress -> Float
progressFactor progress =
    case progress of
        ProgressingSince time ->
            1 / progressTime * time

        Done ->
            0



-- VIEW


calculateOpacity : Model -> Css.Mixin
calculateOpacity { progress } =
    let
        -- value = 1 - (progressFactor progress)
        value =
            1
    in
        opacity (Css.num value)


styles : List Css.Mixin -> Html.Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


view : (Model -> Float -> ( Css.Mixin, Css.Mixin )) -> (Model -> Float -> Css.Mixin) -> Model -> Html msg
view calculateOffsets calculateRotation model =
    let
        factor =
            progressFactor model.progress

        ( topOffset, leftOffset ) =
            calculateOffsets model factor

        rotation =
            calculateRotation model factor

        opacity =
            calculateOpacity model
    in
        div
            [ class "train"
            , styles
                [ opacity
                , topOffset
                , leftOffset
                , rotation
                ]
            ]
            []
