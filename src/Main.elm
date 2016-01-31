module Main (..) where

import Tile
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input   exposing (..)
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


-- type Orientation
--   = North
--   | East
--   | South
--   | West


-- type Kind
--   = Left
--   | Right
--   | Straight


-- type alias Tile =
--   { kind : Kind
--   , orientation : Orientation
--   }


type alias Model = List (ID, Tile.Model)


type alias ID = Int


init : (Model, Effects Action)
init =
  ([], getTick)


-- tileGenerator : Random.Generator Tile
-- tileGenerator =
--   Random.pair (Random.int 0 3) (Random.int 0 3)
--     |> Random.map tile


tilesGenerator : Int -> Random.Generator (List Tile.Model)
tilesGenerator count =
  Random.list count Tile.generator


generateTiles : Int -> Int -> Model
generateTiles count seed =
  Random.generate (tilesGenerator count) (Random.initialSeed seed)
    |> fst
    |> List.indexedMap (,)


-- tile : (Int, Int) -> Tile
-- tile pair =
--   let
--     kindNo =
--       fst pair
--     kind =
--       case kindNo of
--         0 -> Left
--         1 -> Right
--         _ -> Straight
--     orientationNo =
--       snd pair
--     orientation =
--       case orientationNo of
--         0 -> North
--         1 -> East
--         2 -> South
--         _ -> West
--   in
--     Tile kind orientation


-- ACTION


type Action
  = NewTick Int
  | Tile ID Tile.Action


-- UPDATE


woFx : Model -> (Model, Effects Action)
woFx model = (model, Effects.none)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NewTick tick -> (generateTiles 4 tick, Effects.none)
    Tile id tileAction ->
      let updateTile (tileID, tileModel) =
            if tileID == id
            then (tileID, Tile.update tileAction tileModel)
            else (tileID, tileModel)
      in
        List.map updateTile model |> woFx


-- VIEW


size : Int
size = Tile.size * 2


view : Address Action -> Model -> Html.Html
view address model =
  let offset =
        (toFloat Tile.size) / 2
      positions =
        [ (-offset,  offset)
        , (-offset, -offset)
        , ( offset,  offset)
        , ( offset, -offset)
        ]
      align tuple = move (fst tuple) (snd tuple)
      tiles = model
        |> List.map (viewTile address)
        |> List.map2 (,) positions
        |> List.map align
  in
    collage size size tiles
      |> Html.fromElement


viewTile : Address Action -> (ID, Tile.Model) -> Form
viewTile address tuple =
  let id = fst tuple
      tile = snd tuple
  in
    Tile.view (Signal.forwardTo address (Tile id)) tile
