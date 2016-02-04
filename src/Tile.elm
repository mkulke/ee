module Tile (Model, Action, init, view, update, size, generator) where

import Random
import Effects          exposing (Effects)
import Signal           exposing (Address)
import Graphics.Element exposing (..)
import Graphics.Input   exposing (..)
import Graphics.Collage exposing (..)
import Time             exposing (Time)


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


type alias AnimationState = Maybe { previous : Time, elapsed: Time }


type alias Tile =
  { kind : Kind
  , orientation : Orientation
  , animationState: AnimationState
  }


duration : Time
duration = Time.second


type alias Model = Tile


init : (Int, Int) -> Tile
init pair =
  let
    kindNo =
      fst pair
    kind =
      case kindNo of
        0 -> Left
        1 -> Right
        _ -> Straight
    orientationNo =
      snd pair
    orientation =
      case orientationNo of
        0 -> North
        1 -> East
        2 -> South
        _ -> West
  in
    Tile kind orientation Nothing


generator : Random.Generator Tile
generator =
  Random.pair (Random.int 0 3) (Random.int 0 3)
    |> Random.map init


-- ACTION


type Action
  = Rotate
  | Tick Time


-- UPDATE


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Rotate ->
      let newOrientation = case model.orientation of
            North -> East
            East -> South
            South -> West
            West -> North
          effect = case model.animationState of
            Nothing -> Effects.tick Tick
            Just _ -> Effects.none
      in
        ({model | orientation = newOrientation}, effect)
    Tick time ->
      let newElapsed = case model.animationState of
            Nothing -> 0
            Just {elapsed, previous} -> elapsed + (time - previous)
      in
        if newElapsed > duration
        then ( { model | animationState = Nothing }, Effects.none)
        else ( { model | animationState = Just { elapsed = newElapsed
                                               , previous = time } }
             , Effects.tick Tick
             )


-- VIEW


size : Int
size = 50


rotation : Model -> Float
rotation { orientation } =
  case orientation of
    North -> 0
    East -> 90
    South -> 180
    West -> 270


view : Address Action -> Model -> Form
view address model =
  let
    path { kind } =
      case kind of
        Left -> "/img/left.png"
        Right -> "/img/right.png"
        Straight -> "/img/straight.png"
    tileRotate =
      rotate (rotation model |> degrees)
  in
    path model
      |> image size size
      |> Graphics.Input.clickable (Signal.message address Rotate)
      |> toForm
      |> tileRotate
