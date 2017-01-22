module Tile exposing (Model, Msg, init, update, view, subscriptions, generator)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Debounce exposing (bounce)
import Time exposing (Time, millisecond)
import Random exposing (Generator)

-- MODEL


type Orientation
  = North
  | East
  | South
  | West
  | OtherNorth

type Kind
  = Left
  | Right
  | Straight

type alias Model =
  { kind : Kind
  , orientation : Orientation
  , debounce : Debounce.Model Msg
  , bouncing : Bool
  }

init : (Int, Int) -> Model
init pair =
  let
    (kindNo, orientationNo) = pair
    -- x = Debug.log "debug: " ++ (toString kindNo) ++ ":" ++ (toString orientationNo)
    -- x = Debug.log "kind" kindNo
    -- y = Debug.log "orientation" orientationNo
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
    Model kind orientation (Debounce.init debounceTime) False

debounceTime : Time
debounceTime = millisecond * 500

toCSSClasses : Model -> String
toCSSClasses model =
  let
    kind = case model.kind of
      Left -> "left"
      Right -> "right"
      Straight -> "straight"
    orientation = case model.orientation of
      -- No animation to North, since we transition from North' to it.
      North -> "north"
      East -> "east animate"
      South -> "south animate"
      West -> "west animate"
      OtherNorth -> "north-north animate"
  in
    "tile " ++ kind ++ " " ++ orientation

generator : Random.Generator Model
generator =
  Random.pair (Random.int 0 2) (Random.int 0 3)
    |> Random.map init

rotate : Orientation -> Orientation
rotate orientation =
  case orientation of
    North -> East
    East -> South
    South -> West
    West -> OtherNorth
    OtherNorth -> East


-- MSG


type Msg = Rotate
         | DebounceMsg Time
         | ResetBounce


-- UPDATE


update : Msg -> Model -> Model
update msg model =
  case msg of
    ResetBounce ->
      -- When the orientation is North' we need to turn it back to North
      let orientation =
        if model.orientation == OtherNorth
        then North
        else model.orientation
      in
        { model | bouncing = False, orientation = orientation }
    Rotate ->
      if model.bouncing == False then
        { model
        | orientation = rotate(model.orientation)
        , debounce = Debounce.bounce ResetBounce model.debounce
        , bouncing = True
        }
      else model
    DebounceMsg time ->
      let result = Debounce.update time model.debounce
      in
        case result of
          (newDebounce, Nothing) -> { model | debounce = newDebounce }
          (newDebounce, Just newTime) -> update newTime { model | debounce = newDebounce }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Debounce.subscriptions DebounceMsg model.debounce


-- VIEW


view : Model -> Html Msg
view model =
  div [ class (toCSSClasses model)
      , onClick Rotate
      ] []
