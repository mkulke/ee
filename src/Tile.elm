module Tile (Model, Action, init, view, update, size, generator) where

import Random
import Signal           exposing (Address)
import Graphics.Element exposing (..)
import Graphics.Input   exposing (..)
import Graphics.Collage exposing (..)


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


type alias Tile =
  { kind : Kind
  , orientation : Orientation
  }


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
    Tile kind orientation


generator : Random.Generator Tile
generator =
  Random.pair (Random.int 0 3) (Random.int 0 3)
    |> Random.map init


-- ACTION


type Action
  = Rotate


-- UPDATE


update : Action -> Model -> Model
update action model = model


-- VIEW


size : Int
size = 50


view : Address Action -> Model -> Form
view address model =
  let
    path { kind } =
      case kind of
        Left -> "/img/left.png"
        Right -> "/img/right.png"
        Straight -> "/img/straight.png"
    rotation { orientation } =
      case orientation of
        North -> 0
        East -> 90
        South -> 180
        West -> 270
    tileRotate =
      rotate (rotation model |> degrees)
  in
    path model
      |> image size size
      -- |> Graphics.Input.clickable address Rotate
      |> toForm
      |> tileRotate
