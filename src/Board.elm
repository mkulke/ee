module Board exposing (nextTile, moveTrain)
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
  y * boardHeight + boardWidth

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

nextTile : List Tile.Model -> Train.Model -> Maybe Tile.Model
nextTile tiles train =
  let
    max = boardWidth * boardHeight
    index = coordinatesToIndex train.coordinates
    destinationIndex = case train.to of
      Tile.North -> index - boardWidth
      Tile.East -> index + 1
      Tile.South -> index + boardWidth
      Tile.West -> index - 1
  in
    if destinationIndex >= 0 && destinationIndex < max
    then getAt destinationIndex tiles
    else Nothing
