module Main exposing (..)
import Tile
import Html exposing (Html, text)

import Html.App as Html

main = Html.program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

type alias Model = {
  tile : Tile.Model
}

init : (Model, Cmd Msg)
init =
  Model Tile.init ! []


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Sub.map Tile (Tile.subscriptions model.tile) ]


-- MSG


type Msg = Tile Tile.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tile msg' -> { model | tile = Tile.update msg' model.tile } ! []


-- VIEW


view : Model -> Html Msg
view model =
  Html.map Tile (Tile.view model.tile)
-- import Tile
-- import Train
-- import Graphics.Collage exposing (..)
-- import Graphics.Element exposing (..)
-- import Random
-- import StartApp         exposing (start)
-- import Task
-- import Effects          exposing (Effects, Never)
-- import Signal           exposing (Address)
-- import Html
-- import Time


-- -- MAIN


-- app : StartApp.App Model
-- app = start
--   { init = init
--   , update = update
--   , view = view
--   , inputs = [updateTime]
--   }


-- main : Signal Html.Html
-- main = app.html


-- port tasks : Signal (Task.Task Never ())
-- port tasks =
--   app.tasks


-- -- INPUTS


-- updateTime : Signal Action
-- updateTime =
--   Time.every Time.second
--     |> Signal.map UpdateTime


-- -- EFFECTS


-- getTick : Effects Action
-- getTick =
--   Effects.tick round
--     |> Effects.map NewTick


-- -- MODEL


-- type alias Model =
--   { rails : List (ID, Tile.Model)
--   , train : Train.Model
--   }


-- type alias ID = Int


-- init : (Model, Effects Action)
-- init =
--   (Model [] Train.init, getTick)


-- tilesGenerator : Int -> Random.Generator (List Tile.Model)
-- tilesGenerator count =
--   Random.list count Tile.generator


-- generateTiles : Int -> Int -> List (ID, Tile.Model)
-- generateTiles count seed =
--   Random.generate (tilesGenerator count) (Random.initialSeed seed)
--     |> fst
--     |> List.indexedMap (,)


-- size : Int
-- size = 10


-- -- ACTION


-- type Action
--   = NewTick Int
--   | Tile ID Tile.Action
--   | UpdateTime Time.Time


-- -- UPDATE


-- update : Action -> Model -> (Model, Effects Action)
-- update action model =
--   case action of
--     NewTick tick -> ( Model (generateTiles (size * size) tick) Train.init
--                     , Effects.none
--                     )
--     Tile id tileAction ->
--       let updateTile ((tileID, tileModel) as tileTuple) =
--             if tileID == id
--             then
--               let (newTileModel, effect) = Tile.update tileAction tileModel
--               in
--                 ( (id, newTileModel)
--                 , Effects.map (Tile tileID) effect
--                 )
--             else (tileTuple, Effects.none)
--           (newTileModel, effects) =
--             model.rails
--               |> List.map updateTile
--               |> List.unzip
--       in
--         ( { model | rails = newTileModel }
--         , Effects.batch effects
--         )
--     UpdateTime _ ->
--       let (_, rail) = model.rails
--             |> List.drop (size * size - model.train)
--             |> List.head
--             |> Maybe.withDefault (0, (Tile.init (0, 0)))
--           { orientation } = rail
--           modifier = case orientation of
--             Tile.North -> 1
--             Tile.South -> 2
--             Tile.West -> 3
--             Tile.East -> 4
--           position = (model.train + modifier) % 100
--           train' = Train.update (Train.Move position) model.train
--       in
--         ({model | train = train'}, Effects.none)


-- -- VIEW


-- addOffset : (Float, Float) -> (Float, Float)
-- addOffset tuple =
--   let half = (toFloat (size - 1) / 2) * (toFloat Tile.size)
--   in
--     ((fst tuple) - half, (snd tuple) - half)


-- view : Address Action -> Model -> Html.Html
-- view address model =
--   let zip index = List.map2 (,) (List.repeat size (index * Tile.size))
--       positions = [0..size - 1]
--         |> List.map ((*) Tile.size) -- [0,50,100]
--         |> List.repeat size         -- [[0,50,100],...]
--         |> List.indexedMap zip      -- [[(0,0),(0,50),(0,100)],...]
--         |> List.concat              -- [(0,0),(0,50),(0,100),(50,0)...]
--         |> List.map (\(x, y)-> (toFloat x, toFloat y))
--         |> List.map addOffset
--       tiles = model.rails
--         |> List.map (viewTile address)
--       align forms = forms
--         |> List.map2 (,) positions
--         |> List.map (\(x, y) -> move x y)
--         |> collage (Tile.size * size) (Tile.size * size)
--       rails = tiles
--         |> align
--       train = positions
--         |> List.drop (size * size - model.train)
--         |> List.head
--         |> Maybe.withDefault (0, 0)
--         |> (\position -> move position (Train.view model.train))
--         |> (\form -> form :: [])
--         |> collage (Tile.size * size) (Tile.size * size)
--   in
--     layers [train, rails]
--       |> Html.fromElement


-- viewTile : Address Action -> (ID, Tile.Model) -> Form
-- viewTile address tuple =
--   let (id, tile) = tuple
--   in
--     Tile.view (Signal.forwardTo address (Tile id)) tile
