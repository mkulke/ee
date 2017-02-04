module Board exposing (nextIndex, getNewDirections, tilesGenerator, calculateOffsets, fixFirstTile)
import Tile
import Train
import Maybe
import Random exposing (generate)
import Css exposing (px, left, top)

boardWidth : Int
boardWidth = 6

boardHeight : Int
boardHeight = 6

-- the first tile should not point to either North or West
fixFirstTile : List Tile.Model -> List Tile.Model
fixFirstTile tiles =
  let
    rotateTwice = Tile.rotateDirection >> Tile.rotateDirection
    turnAround = \tile -> { tile | orientation = rotateTwice tile.orientation }
    fix = \tile -> case Tile.connections tile of
      (_, Tile.North) -> turnAround tile
      (_, Tile.West) -> turnAround tile
      _ -> tile
  in
    case tiles of
      firstTile :: otherTiles -> (fix firstTile) :: otherTiles
      [] -> []

tilesGenerator : Random.Generator (List Tile.Model)
tilesGenerator =
  Random.list (boardWidth * boardHeight) Tile.generator

indexToCoordinates : Int -> (Int, Int)
indexToCoordinates index =
  (index % boardWidth, index // boardHeight)

getNewDirections : Train.Model -> Tile.Model -> Maybe (Tile.Direction, Tile.Direction)
getNewDirections train tile =
  let
    trainDirection = train.to
    rotateTwice = Tile.rotateDirection >> Tile.rotateDirection
    trainConnection = rotateTwice trainDirection
    (oneEnd, otherEnd) = Tile.connections tile
  in
    if trainConnection == oneEnd then Just (oneEnd, otherEnd)
    else if trainConnection == otherEnd then Just (otherEnd, oneEnd)
    else Nothing

calculateOffsets : Int -> List Css.Mixin
calculateOffsets index =
  let
    coordinates = indexToCoordinates index
    x = toFloat (Tuple.first coordinates) * 80
    y = toFloat (Tuple.second coordinates) * 80
  in
    [top (px y), left (px x)]

nextIndex : Train.Model -> Maybe Int
nextIndex train =
  let
    max = boardWidth * boardHeight
    index = train.index
  in
    case train.to of
      Tile.North -> if index - boardWidth > 0 then Just (index - boardWidth) else Nothing
      Tile.East -> if ((index + 1) % boardWidth) > 0 then Just (index + 1) else Nothing
      Tile.South -> if index + boardWidth < max then Just (index + boardWidth) else Nothing
      Tile.West -> if (index % boardWidth) > 0 then Just (index - 1) else Nothing
