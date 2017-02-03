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
import Css exposing (asPairs, px, left, top)

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
      firstTile :: _ -> Just (Train.init index firstTile)
      [] -> Nothing

updateTrainIndex : Int -> Train.Model -> Train.Model
updateTrainIndex index train =
  { train | index = index }

updateTrain : Train.Model -> Int -> (Tile.Direction, Tile.Direction) -> Train.Model
updateTrain train index (from, to) =
  { train | from = from, to = to, index = index }

bla : List Tile.Model -> Train.Model -> Maybe Train.Model
bla tiles train =
  let
    x = getAt train.index tiles
    y = Debug.log "mgns" (train.index, x)
    --   |> Maybe.andThen (Board.getNewDirections train)
    --   |> Maybe.map (\(from, to) -> { train | from = from, to = to })
  in
    -- x
    Nothing

moveTrain : List Tile.Model -> Train.Model -> Train.Model
moveTrain tiles train =
  let
    x = Board.nextIndex train
      |> Maybe.map (\index -> { train | index = index })
      |> Maybe.andThen (bla tiles)
  in
    x |> withDefault train

updateTrainTick : Float -> Model -> Train.Model -> Train.Model
updateTrainTick diff model train =
  let
    progressedTrain = Train.updateProgress diff train
  in
    case progressedTrain.progress of
      Train.ProgressingSince _ -> progressedTrain
      -- Train.Done -> Debug.log "nextIndex" (Board.nextIndex newTrain)
      Train.Done -> moveTrain model.tiles progressedTrain

--     nextIndex = case newTrain.progress of
--       Train.ProgressingSince _ -> -1
--       Train.Done -> Debug.log "nextIndex" (Board.nextIndex newTrain)
--     nextTile = getAt nextIndex model.tiles
--     x = case nextTile of
--       Nothing -> newTrain
--       Just tile ->
--         let
--           movedTrain = Board.moveTrain newTrain tile
--           blaTrain = Maybe.map (updateTrainIndex nextIndex) movedTrain
--         in
--           blaTrain |> withDefault newTrain
--   in
--     newTrain

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

calculateOffsets : Train.Model -> List Css.Mixin
calculateOffsets { index } =
  let
    coordinates = indexToCoordinates index
    x = toFloat (Tuple.first coordinates) * 80
    y = toFloat (Tuple.second coordinates) * 80
  in
    [top (px y), left (px x)]

styles : List Css.Mixin -> Html.Attribute msg
styles =
  Css.asPairs >> Html.Attributes.style

view : Model -> Html Msg
view model =
  let
    tileView = \index tile -> Html.map (Tile index) (Tile.debugView tile index)
    trainStyles = Maybe.map calculateOffsets model.train |> withDefault []
    trainView = \train -> div [ class "bla", styles trainStyles ] [ Train.view train ]
    tilesView = List.indexedMap tileView model.tiles
    tileRows = groupsOf 6 tilesView |> List.map (div [])
    divs = case model.train of
      Nothing -> tileRows
      Just train -> trainView train :: tileRows
  in
    div [ class "grid" ] divs
