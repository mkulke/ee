module Board exposing (nextTile, nextIndex, moveTrain, getNewDirections, hasContact)
import Tile
import Train
import Maybe
import List.Extra exposing (getAt)

boardWidth : Int
boardWidth = 6

boardHeight : Int
boardHeight = 6

coordinatesToIndex : (Int, Int) -> Int
coordinatesToIndex (x, y) =
  y * boardHeight + boardWidth * x

hasContact : Train.Model -> Tile.Model -> Bool
hasContact train tile =
  False

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

moveTrain : Train.Model -> Tile.Model -> Maybe Train.Model
moveTrain train tile =
  let
    trainDirection = train.to
    rotateTwice = Tile.rotateDirection >> Tile.rotateDirection
    trainConnection = rotateTwice trainDirection
    (oneEnd, otherEnd) = Tile.connections tile
  in
    if trainConnection == oneEnd then Just { train | to = otherEnd, from = oneEnd }
    else if trainConnection == otherEnd then Just { train | to = oneEnd, from = otherEnd }
    else Nothing

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

nextTile : List Tile.Model -> Train.Model -> Maybe Tile.Model
nextTile tiles train =
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
    then getAt destinationIndex tiles
    else Nothing
