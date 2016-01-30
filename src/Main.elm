module Main (..) where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Random
import StartApp         exposing (start)
import Task
import Effects          exposing (Effects, Never)
import Signal           exposing (Address)
import Html
-- import Debug


-- MAIN


app : StartApp.App Model
app = start
  { init = init
  , update = update
  , view = view
  , inputs = []
  }


main : Signal Html.Html
main = app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks


-- EFFECTS


getTick : Effects Action
getTick =
  Effects.tick round
    |> Effects.map NewTick


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


type alias Model = List Tile


init : (Model, Effects Action)
init =
  ([], getTick)


tileGenerator : Random.Generator Tile
tileGenerator =
  Random.pair (Random.int 0 3) (Random.int 0 3)
    |> Random.map tile


listOfTilesGenerator : Int -> Random.Generator (List Tile)
listOfTilesGenerator count =
  Random.list count tileGenerator


generateListOfTiles : Int -> Int -> (List Tile)
generateListOfTiles count seed =
  Random.generate (listOfTilesGenerator count) (Random.initialSeed seed)
    |> fst


tile : (Int, Int) -> Tile
tile pair =
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


-- ACTION


type Action = NewTick Int


-- UPDATE


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NewTick tick -> (generateListOfTiles 4 tick, Effects.none)


-- VIEW


view : Address Action -> Model -> Html.Html
view address model =
  let
    offset =
      (toFloat tileSize) / 2
    positions =
      [ (-offset,  offset)
      , (-offset, -offset)
      , ( offset,  offset)
      , ( offset, -offset)
      ]
    align tuple = move (fst tuple) (snd tuple)
    tiles = model
      |> List.map tileView
      |> List.map2 (,) positions
      |> List.map align
  in
    collage
      boardSize
      boardSize
      tiles
    |> Html.fromElement


tileSize : Int
tileSize = 50


boardSize : Int
boardSize = tileSize * 2


tileView : Tile -> Form
tileView tile =
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
      rotate (rotation tile |> degrees)
  in
    path tile
      |> image tileSize tileSize
      |> toForm
      |> tileRotate
