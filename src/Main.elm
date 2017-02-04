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
  Model [] Nothing ! [Random.generate NewRandom Board.tilesGenerator]

type alias Index = Int


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
  in
    case tilesTail of
      firstTile :: _ -> Just (Train.init index firstTile)
      [] -> Nothing

moveTrain : List Tile.Model -> Train.Model -> Train.Model
moveTrain tiles train =
  let
    assertIdle = \tile -> if Tile.idle tile then Just tile else Nothing
    setIndex = \index -> { train | index = index }
    setDirection = \newTrain -> getAt newTrain.index tiles
      |> Maybe.andThen assertIdle
      |> Maybe.andThen (Board.getNewDirections newTrain)
      |> Maybe.map (\(from, to) -> { newTrain | from = from, to = to })
  in
    Board.nextIndex train
      |> Maybe.map setIndex
      |> Maybe.andThen setDirection
      |> withDefault train

updateTrainTick : Float -> Model -> Train.Model -> Train.Model
updateTrainTick diff model train =
  let
    progressedTrain = Train.updateProgress diff train
  in
    case progressedTrain.progress of
      Train.ProgressingSince _ -> progressedTrain
      Train.Done -> moveTrain model.tiles progressedTrain

updateTick : Float -> Model -> Model
updateTick diff model =
  let
    updateTrain = updateTrainTick diff model
  in
    { model
    | tiles = List.map (Tile.updateTick diff) model.tiles
    , train = Maybe.map updateTrain model.train
    }

populateBoard : Model -> List Tile.Model -> Model
populateBoard model tiles =
  let
    fixedTiles = Board.fixFirstTile tiles
  in
    { model | tiles = fixedTiles, train = initTrain fixedTiles 0 }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tile index tileMsg -> { model | tiles = updateTiles tileMsg index model.tiles } ! []
    Tick diff -> updateTick diff model ! []
    NewRandom tiles -> populateBoard model tiles ! []


-- VIEW

view : Model -> Html Msg
view model =
  let
    tileView = \index tile -> Html.map (Tile index) (Tile.debugView tile index)
    trainView = \train -> Train.view Board.calculateOffsets train
    tilesView = List.indexedMap tileView model.tiles
    tileRows = groupsOf 6 tilesView |> List.map (div [])
    divs = case model.train of
      Nothing -> tileRows
      Just train -> trainView train :: tileRows
  in
    div [ class "grid" ] divs
