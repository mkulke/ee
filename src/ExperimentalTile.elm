module ExperimentalTile exposing (Model, Msg, updateTick, init, update, view, generator)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Css exposing (asPairs, transform, deg, rotate)
import Random exposing (Generator)
import String exposing (join)


-- MODEL

type Orientation
  = North
  | East
  | South
  | West

type Kind
  = Left
  | Right
  | Straight

type alias Model =
  { kind: Kind
  , orientation: Orientation
  , transitioning: Transition
  }

type Transition = NotTransitioning | TransitioningSince Float

generator : Random.Generator Model
generator =
  Random.pair (Random.int 0 2) (Random.int 0 3)
    |> Random.map init

init : (Int, Int) -> Model
init (kindNo, orientationNo) =
  let
    kind = case kindNo of
      0 -> Left
      1 -> Right
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


-- MSG

type Msg = Rotate


-- UPDATE

newOrientation : Model -> Orientation
newOrientation { orientation } =
  case orientation of
    North -> East
    East -> South
    South -> West
    West -> North

update : Msg -> Model -> Model
update msg model =
  case msg of
    Rotate ->
      if model.transitioning == NotTransitioning
      then { model | transitioning = TransitioningSince 0
                   , orientation = newOrientation model
           }
      else model


-- VIEW

styles : List Css.Mixin -> Html.Attribute msg
styles =
  Css.asPairs >> Html.Attributes.style

rotation : Model -> Css.Mixin
rotation { orientation, transitioning } =
  let
    position = case orientation of
      North -> 0
      East -> 90
      South -> 180
      West -> 270
    transitionOffset = position - 90
    toFraction = \time -> transitionOffset + (90 / transitionTime * time)
    degree = case transitioning of
      NotTransitioning -> deg position
      TransitioningSince time -> toFraction time |> deg
  in
    transform (rotate degree)

toCSSClasses : Model -> List String
toCSSClasses { kind, orientation } =
  let
    kindClass = case kind of
      Left -> "left"
      Right -> "right"
      Straight -> "straight"
    orientationClass = case orientation of
      North -> "north"
      East -> "east"
      South -> "south"
      West -> "west"
  in
    [kindClass, orientationClass]

view : Model -> Html Msg
view model =
  div [ class (join " " ("tile" :: toCSSClasses model))
      , styles [ rotation model ]
      , onClick Rotate
      ] []
