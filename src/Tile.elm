module Tile (Model, Action, init, view, update, size, generator) where

import Random
import Effects          exposing (Effects)
import Signal           exposing (Address)
import Graphics.Element exposing (..)
import Graphics.Input   exposing (..)
import Graphics.Collage exposing (..)
import Color            exposing (..)
import Time             exposing (Time)
import Easing           exposing (float, ease, easeOutBounce)


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


type alias EvolvingState = Maybe { previous : Time, elapsed: Time }


type alias Tile =
  { kind : Kind
  , orientation : Orientation
  , animationState: EvolvingState
  -- , occupationState: EvolvingState
  , test: Color
  }


duration : Time
duration = Time.second


type alias Model = Tile


init : (Int, Int) -> Tile
init pair =
  let (kindNo, orientationNo) = pair
      kind =
        case kindNo of
          0 -> Left
          1 -> Right
          _ -> Straight
      orientation =
        case orientationNo of
          0 -> North
          1 -> East
          2 -> South
          _ -> West
      color = if orientationNo == 0 then Color.blue else Color.lightBlue
  in
    Tile kind orientation Nothing color


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
      let effect = case model.animationState of
        Nothing -> Effects.tick Tick
        Just _ -> Effects.none
      in
        (model, effect)
    Tick time ->
      let elapsed' = case model.animationState of
            Nothing -> 0
            Just {elapsed, previous} -> elapsed + (time - previous)
          orientation' = case model.orientation of
            North -> East
            East -> South
            South -> West
            West -> North
      in
        if elapsed' > duration
        then ( { model | animationState = Nothing
                       , orientation = orientation'
                       }
             , Effects.none
             )
        else
          let animationState' = Just { elapsed = elapsed', previous = time }
          in
            ( { model | animationState = animationState' }
            , Effects.tick Tick
            )


-- VIEW


size : Int
size = 50


rotation : Model -> Float
rotation { orientation, animationState } =
  let base = case orientation of
        North -> 0
        East -> 90
        South -> 180
        West -> 270
      toEased elapsed = ease easeOutBounce float 0 90 duration elapsed
  in
    case animationState of
      Nothing -> base
      Just { elapsed } -> base + toEased elapsed


view : Address Action -> Model -> (Form, Form)
view address model =
  let path { kind } =
        case kind of
          Left -> "/img/left.png"
          Right -> "/img/right.png"
          Straight -> "/img/straight.png"
      tileRotate =
        rotate (rotation model |> degrees)
      tileImage = path model
        |> image size size
        |> Graphics.Input.clickable (Signal.message address Rotate)
        |> toForm
        |> tileRotate
      tileBg = square (toFloat size)
        |> filled model.test
  in
    (tileImage, tileBg)
