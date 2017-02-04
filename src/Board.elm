module Board exposing (nextIndex, getNewDirections, tilesGenerator, calculateOffsets)
import Tile
import Train
import Maybe
import Random exposing (generate)
import Css exposing (px, left, top)

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
    destinationIndex = case train.to of
      Tile.North -> index - boardWidth
      Tile.East -> index + 1
      Tile.South -> index + boardWidth
      Tile.West -> index - 1
  in
    if destinationIndex >= 0 && destinationIndex < max
    then Just destinationIndex
    else Nothing
