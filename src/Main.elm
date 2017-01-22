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


-- MODEL

type alias Model =
  { tiles: List ExperimentalTile.Model }

init : (Model, Cmd Msg)
init =
  Model [] ! [Random.generate NewRandom tilesGenerator]

type alias Index = Int

tilesGenerator : Random.Generator (List ExperimentalTile.Model)
tilesGenerator =
  Random.list 36 ExperimentalTile.generator


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  AnimationFrame.diffs Tick


-- MSG

type Msg = ExperimentalTile Index ExperimentalTile.Msg
         | Tick Float
         | NewRandom (List ExperimentalTile.Model)

updateTiles : ExperimentalTile.Msg -> Index -> List ExperimentalTile.Model -> List ExperimentalTile.Model
updateTiles msg index tiles =
  let
    tileUpdate = ExperimentalTile.update msg
    getTile = getAt index tiles
    setTile = \tile -> setAt index (tileUpdate tile) tiles
  in
    getTile |> andThen setTile |> withDefault tiles

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ExperimentalTile index experimentalTileMsg -> { model | tiles = updateTiles experimentalTileMsg index model.tiles } ! []
    Tick diff -> { model | tiles = List.map (ExperimentalTile.updateTick diff) model.tiles } ! []
    NewRandom tiles -> { model | tiles = tiles } ! []


-- VIEW

view : Model -> Html Msg
view model =
  let
    tileView = \index tile -> div [] [ Html.map (ExperimentalTile index) (ExperimentalTile.view tile) ]
    tilesView = List.indexedMap tileView model.tiles
    tileRows = groupsOf 6 tilesView |> List.map (\row -> div [ class "float" ] row)
  in
    div [ class "grid" ] tileRows

