module Main exposing (..)
import Tile
import Train
import Board
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
  { tiles: List Tile.Model
  , train: Maybe Train.Model
  }

init : (Model, Cmd Msg)
init =
  Model [] Nothing ! [Random.generate NewRandom tilesGenerator]

type alias Index = Int

boardWidth : Int
boardWidth = 6

boardHeight : Int
boardHeight = 6

tilesGenerator : Random.Generator (List Tile.Model)
tilesGenerator =
  Random.list (boardWidth * boardHeight) Tile.generator

indexToCoordinates : Int -> (Int, Int)
indexToCoordinates index =
  (index % boardWidth, index // boardHeight)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  AnimationFrame.diffs Tick


-- MSG

type Msg = Tile Index Tile.Msg
         | Tick Float
         | NewRandom (List Tile.Model)

updateTiles : Tile.Msg -> Index -> List Tile.Model -> List Tile.Model
updateTiles msg index tiles =
  let
    tileUpdate = Tile.update msg
    getTile = getAt index tiles
    setTile = \tile -> setAt index (tileUpdate tile) tiles
  in
    getTile |> andThen setTile |> withDefault tiles

initTrain : List Tile.Model -> Int -> Maybe Train.Model
initTrain tiles index =
  let
    tilesTail = List.drop index tiles
    coordinates = indexToCoordinates index
  in
    case tilesTail of
      firstTile :: _ -> Just (Train.init coordinates firstTile)
      [] -> Nothing

updateTrainTick : Float -> Model -> Train.Model -> Train.Model
updateTrainTick diff model train =
  let
    newTrain = Train.updateProgress diff train
    -- currentTile = newTrain.tile
    -- nextTile = case newTrain.progress of
    --   Train.ProgressingSince _ -> Nothing
    --   Train.Done -> Debug.log "nextTile" (Board.nextTile model.tiles newTrain)
  in
    newTrain

updateTick : Float -> Model -> Model
updateTick diff model =
  let
    updateTrain = updateTrainTick diff model
  in
    { model
    | tiles = List.map (Tile.updateTick diff) model.tiles
    , train = Maybe.map updateTrain model.train
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tile index tileMsg -> { model | tiles = updateTiles tileMsg index model.tiles } ! []
    Tick diff -> updateTick diff model ! []
    NewRandom tiles -> { model | tiles = tiles, train = initTrain tiles 0 } ! []


-- VIEW

view : Model -> Html Msg
view model =
  let
    tileView = \index tile -> Html.map (Tile index) (Tile.view tile)
    trainView = \train -> Train.view train
    tilesView = List.indexedMap tileView model.tiles
    tileRows = groupsOf 6 tilesView |> List.map (\row -> div [ class "float" ] row)
    divs = case model.train of
      Nothing -> tileRows
      Just train -> trainView train :: tileRows
  in
    div [ class "grid" ] divs
