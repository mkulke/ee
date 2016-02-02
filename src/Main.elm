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


size : Int
size = 10


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
    NewTick tick -> (generateTiles (size * size) tick, Effects.none)
    Tile id tileAction ->
      let updateTile (tileID, tileModel) =
            if tileID == id
            then (tileID, Tile.update tileAction tileModel)
            else (tileID, tileModel)
      in
        List.map updateTile model |> noFx


-- VIEW


addOffset : (Float, Float) -> (Float, Float)
addOffset tuple =
  let half = (toFloat (size - 1) / 2) * (toFloat Tile.size)
  in
    ((fst tuple) - half, (snd tuple) - half)


view : Address Action -> Model -> Html.Html
view address model =
  let zip index = List.map2 (,) (List.repeat size (index * Tile.size))
      positions = [0..size - 1]
        |> List.map ((*) Tile.size) -- [0,50,100]
        |> List.repeat size         -- [[0,50,100],...]
        |> List.indexedMap zip      -- [[(0,0),(0,50),(0,100)],...]
        |> List.concat              -- [(0,0),(0,50),(0,100),(50,0)...]
        |> List.map (\t -> (toFloat (fst t), toFloat (snd t)))
        |> List.map addOffset
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
