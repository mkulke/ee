module Tile exposing (Model, Msg, Direction(..), idle, connections, updateTick, init, update, debugView, view, generator, rotateDirection)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Css exposing (asPairs, transform, deg, rotate)
import Random exposing (Generator)
import String exposing (join)


-- MODEL

type Direction
  = North
  | East
  | South
  | West

type alias Orientation = Direction

type Kind
  = Turn
  | Straight

type alias Model =
  { kind: Kind
  , orientation: Orientation
  , transitioning: Transition
  }

type Transition = NotTransitioning | TransitioningSince Float

generator : Random.Generator Model
generator =
  Random.pair (Random.int 0 1) (Random.int 0 3)
    |> Random.map init

init : (Int, Int) -> Model
init (kindNo, orientationNo) =
  let
    kind = case kindNo of
      0 -> Turn
      _ -> Straight
    orientation = case orientationNo of
      0 -> North
      1 -> East
      2 -> South
      _ -> West
  in
    Model kind orientation NotTransitioning

updateTick : Float -> Model -> Model
updateTick diff model =
  case model.transitioning of
    NotTransitioning -> model
    TransitioningSince time ->
      { model | transitioning =
        if time < transitionTime
        then TransitioningSince (time + diff)
        else NotTransitioning
      }

transitionTime : Float
transitionTime = 500

idle : Model -> Bool
idle model =
  model.transitioning == NotTransitioning

mapTuple : (a -> a) -> (a, a) -> (a, a)
mapTuple fn tuple =
  (fn (Tuple.first tuple), fn (Tuple.second tuple))

connections : Model -> (Direction, Direction)
connections { kind, orientation } =
  let
    noOfRotations = rotationFactor orientation
    rotate = \tuple no -> if no == 0
                          then tuple
                          else rotate (mapTuple rotateDirection tuple) (no - 1)
    unRotatedConnections = case kind of
      Straight -> (North, South)
      Turn -> (North, West)
  in
    rotate unRotatedConnections noOfRotations

rotationFactor : Orientation -> Int
rotationFactor orientation =
  case orientation of
    North -> 0
    East -> 1
    South -> 2
    West -> 3

rotateDirection : Direction -> Direction
rotateDirection direction =
  case direction of
    North -> East
    East -> South
    South -> West
    West -> North


-- MSG

type Msg = Rotate


-- UPDATE

update : Msg -> Model -> Model
update msg model =
  case msg of
    Rotate ->
      if model.transitioning == NotTransitioning
      then { model | transitioning = TransitioningSince 0
                   , orientation = rotateDirection model.orientation
           }
      else model


-- VIEW

styles : List Css.Mixin -> Html.Attribute msg
styles =
  Css.asPairs >> Html.Attributes.style

rotation : Model -> Css.Mixin
rotation { orientation, transitioning } =
  let
    factor = rotationFactor orientation
    step = 90
    position = toFloat (factor * step)
    transitionOffset = position - step
    toFraction = \time -> transitionOffset + (step / transitionTime * time)
    degree = case transitioning of
      NotTransitioning -> deg position
      TransitioningSince time -> toFraction time |> deg
  in
    transform (rotate degree)

toCSSClasses : Model -> List String
toCSSClasses { kind, orientation } =
  let
    kindClass = kind |> toString |> String.toLower
    orientationClass = orientation |> toString |> String.toLower
  in
    [kindClass, orientationClass]

debugView : Model -> Int -> Html Msg
debugView model index =
  div [ class (join " " ("tile" :: toCSSClasses model))
      , styles [ rotation model ]
      , onClick Rotate
      ] [ text (toString index) ]

view : Model -> Html Msg
view model =
  div [ class (join " " ("tile" :: toCSSClasses model))
      , styles [ rotation model ]
      , onClick Rotate
      ] []
