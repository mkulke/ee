module Main exposing (..)
import Tile
import ExperimentalTile
import Html exposing (Html, text, div)
import Html.Attributes exposing (class)
import Random exposing (generate)
import Maybe exposing (withDefault, andThen)
import List.Extra exposing (getAt, setAt, groupsOf)
import AnimationFrame

main : Program Never Model Msg
main = Html.program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

type alias Model =
  { tiles: List Tile.Model
  , experimentalTiles: List ExperimentalTile.Model
  }

init : (Model, Cmd Msg)
init =
  let experimentalTiles = List.map ExperimentalTile.init (List.range 1 36)
  in
    Model [] experimentalTiles ! [Random.generate NewRandom tilesGenerator]

type alias Index = Int

tilesGenerator : Random.Generator (List Tile.Model)
tilesGenerator =
  Random.list 36 Tile.generator


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    tileSub = \index tile -> Sub.map (Tile index) (Tile.subscriptions tile)
    tileSubs = List.indexedMap tileSub model.tiles
    tick = AnimationFrame.diffs Tick
  in
    Sub.batch (tick :: tileSubs)
    -- Sub.batch tileSubs


-- MSG

type Msg = Tile Index Tile.Msg
         | ExperimentalTile Index ExperimentalTile.Msg
         | Tick Float
         | NewRandom (List Tile.Model)


-- UPDATE

updateTiles : Tile.Msg -> Index -> List Tile.Model -> List Tile.Model
updateTiles msg index tiles =
  let
    tileUpdate = Tile.update msg
    getTile = getAt index tiles
    setTile = \tile -> setAt index (tileUpdate tile) tiles
  in
    getTile |> andThen setTile |> withDefault tiles

updateExperimentalTiles : ExperimentalTile.Msg -> Index -> List ExperimentalTile.Model -> List ExperimentalTile.Model
updateExperimentalTiles msg index tiles =
  let
    tileUpdate = ExperimentalTile.update msg
    getTile = getAt index tiles
    setTile = \tile -> setAt index (tileUpdate tile) tiles
  in
    getTile |> andThen setTile |> withDefault tiles

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tile index tileMsg -> { model | tiles = updateTiles tileMsg index model.tiles } ! []
    ExperimentalTile index experimentalTileMsg -> { model | experimentalTiles = updateExperimentalTiles experimentalTileMsg index model.experimentalTiles } ! []
    -- ExperimentalTile index newMsg -> model ! []
    Tick diff -> { model | experimentalTiles = List.map (ExperimentalTile.updateTick diff) model.experimentalTiles } ! []
    NewRandom tiles -> { model | tiles = tiles } ! []


-- VIEW

view : Model -> Html Msg
view model =
  let
    -- tileView = \index tile -> div [] [ Html.map (Tile index) (Tile.view tile) ]
    tileView = \index tile -> div [] [ Html.map (ExperimentalTile index) (ExperimentalTile.view tile) ]
    tilesView = List.indexedMap tileView model.experimentalTiles
    tileRows = groupsOf 6 tilesView |> List.map (\row -> div [ class "float" ] row)
  in
    div [ class "grid" ] tileRows

