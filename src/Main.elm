module Main (..) where

import Tile
import Graphics.Collage exposing (..)
import Random
import StartApp         exposing (start)
import Task
import Effects          exposing (Effects, Never)
import Signal           exposing (Address)
import Html
import Debug


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


type alias Model = List (ID, Tile.Model)


type alias ID = Int


init : (Model, Effects Action)
init =
  ([], getTick)


tilesGenerator : Int -> Random.Generator (List Tile.Model)
tilesGenerator count =
  Random.list count Tile.generator


generateTiles : Int -> Int -> Model
generateTiles count seed =
  Random.generate (tilesGenerator count) (Random.initialSeed seed)
    |> fst
    |> List.indexedMap (,)


-- ACTION


type Action
  = NewTick Int
  | Tile ID Tile.Action


-- UPDATE


noFx : Model -> (Model, Effects Action)
noFx model = (model, Effects.none)


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
        List.map updateTile model |> noFx


-- VIEW


size : Int
size = 2


addOffset : (Float, Float) -> (Float, Float)
addOffset tuple =
  let half = (toFloat Tile.size) / 2
  in
    ((fst tuple) - half, (snd tuple) - half)


view : Address Action -> Model -> Html.Html
view address model =
  let whack y = List.indexedMap (\i t -> (i * Tile.size, y)) [1..size]
      whock = List.indexedMap (\i t -> whack (i * Tile.size)) [1..size]
      whick = Debug.log "whick" (List.concat whock)
      whuck = List.map (\x -> (toFloat (fst x), toFloat (snd x))) whick
      positions = List.map addOffset whuck
      align tuple = move (fst tuple) (snd tuple)
      tiles = model
        |> List.map (viewTile address)
        |> List.map2 (,) positions
        |> List.map align
  in
    collage (Tile.size * size) (Tile.size * size) tiles
      |> Html.fromElement


viewTile : Address Action -> (ID, Tile.Model) -> Form
viewTile address tuple =
  let id = fst tuple
      tile = snd tuple
  in
    Tile.view (Signal.forwardTo address (Tile id)) tile
