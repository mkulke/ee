module Tile exposing (Direction(..), Model, Msg, Occupancy(..), connections, debugView, generator, getOccupancy, idle, init, rotateDirection, setOccupancy, update, updateTick, view)

import Css exposing (deg, rotate, transform)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (class, css)
import Html.Styled.Events exposing (onClick)
import Random exposing (Generator)
import String exposing (join)



-- MODEL


type Occupancy
    = Vacant
    | Blocked


type Direction
    = North
    | East
    | South
    | West


type alias Orientation =
    Direction


type Kind
    = Turn
    | Straight


type alias Model =
    { kind : Kind
    , orientation : Orientation
    , transitioning : Transition
    , occupancy : Occupancy
    }


type Transition
    = NotTransitioning
    | TransitioningSince Float


generator : Random.Generator Model
generator =
    Random.pair (Random.int 0 1) (Random.int 0 3)
        |> Random.map init


init : ( Int, Int ) -> Model
init ( kindNo, orientationNo ) =
    let
        kind =
            case kindNo of
                0 ->
                    Turn

                _ ->
                    Straight

        orientation =
            case orientationNo of
                0 ->
                    North

                1 ->
                    East

                2 ->
                    South

                _ ->
                    West
    in
    Model kind orientation NotTransitioning Vacant


getOccupancy : Model -> Occupancy
getOccupancy model =
    model.occupancy


setOccupancy : Occupancy -> Model -> Model
setOccupancy occupancy model =
    { model | occupancy = occupancy }


updateTick : Float -> Model -> Model
updateTick diff model =
    case model.transitioning of
        NotTransitioning ->
            model

        TransitioningSince time ->
            { model
                | transitioning =
                    if time < transitionTime then
                        TransitioningSince (time + diff)

                    else
                        NotTransitioning
            }


transitionTime : Float
transitionTime =
    500


idle : Model -> Bool
idle model =
    model.transitioning == NotTransitioning


mapTuple : (a -> a) -> ( a, a ) -> ( a, a )
mapTuple fn tuple =
    ( fn (Tuple.first tuple), fn (Tuple.second tuple) )


connections : Model -> ( Direction, Direction )
connections { kind, orientation } =
    let
        noOfRotations =
            rotationFactor orientation

        rotate90 tuple no =
            case no of
                0 ->
                    tuple

                _ ->
                    rotate90 (mapTuple rotateDirection tuple) (no - 1)

        unRotatedConnections =
            case kind of
                Straight ->
                    ( North, South )

                Turn ->
                    ( North, West )
    in
    rotate90 unRotatedConnections noOfRotations


kindToString : Kind -> String
kindToString value =
    case value of
        Turn ->
            "turn"

        Straight ->
            "straight"


rotationFactor : Orientation -> Int
rotationFactor orientation =
    case orientation of
        North ->
            0

        East ->
            1

        South ->
            2

        West ->
            3


rotateDirection : Direction -> Direction
rotateDirection direction =
    case direction of
        North ->
            East

        East ->
            South

        South ->
            West

        West ->
            North



-- MSG


type Msg
    = Rotate



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        Rotate ->
            -- let
            --     _ =
            --         Debug.log "mgns" model
            -- in
            if model.transitioning == NotTransitioning && model.occupancy == Vacant then
                { model
                    | transitioning = TransitioningSince 0
                    , orientation = rotateDirection model.orientation
                }

            else
                model


rotation : Model -> Css.Style
rotation { orientation, transitioning } =
    let
        factor =
            rotationFactor orientation

        step =
            90

        position =
            toFloat (factor * step)

        transitionOffset =
            position - step

        toFraction =
            \time -> transitionOffset + (step / transitionTime * time)

        degree =
            case transitioning of
                NotTransitioning ->
                    deg position

                TransitioningSince time ->
                    toFraction time |> deg
    in
    transform (rotate degree)


toCSSClasses : Model -> List String
toCSSClasses { kind } =
    [ kindToString kind ]


debugView : Model -> Int -> Html Msg
debugView model index =
    div
        [ class (join " " ("tile" :: toCSSClasses model))
        , css [ rotation model ]
        , onClick Rotate
        ]
        [ text (String.fromInt index) ]


view : Model -> Html Msg
view model =
    div
        [ class (join " " ("tile" :: toCSSClasses model))
        , css [ rotation model ]
        , onClick Rotate
        ]
        []
