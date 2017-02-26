module Board exposing (nextIndex, getNewDirections, tilesGenerator, calculateOffsets, tilesOk)
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
tilesOk : List Tile.Model -> Bool
tilesOk tiles =
  let
    pointsNorthOrWest = \tile -> case Tile.connections tile of
      (_, Tile.North) -> False
      (_, Tile.West) -> False
      _ -> True
  in
    case tiles of
      firstTile :: otherTiles -> pointsNorthOrWest firstTile
      [] -> False

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

calculateXTurn : Float -> Float -> Float
calculateXTurn delta factor =
  let
    angle = turns (factor / 4)
  in
    delta * cos angle

calculateYTurn : Float -> Float -> Float
calculateYTurn delta factor =
  let
    angle = turns (factor / 4)
  in
    delta * sin angle


calculateDelta : (Tile.Direction, Tile.Direction) -> Float -> (Float, Float)
calculateDelta movement progressFactor =
  let
    angleOffset = -0.25
  in
    case movement of
      (Tile.North, Tile.South) -> (0, 80 * progressFactor - 40)
      (Tile.North, Tile.East) ->
        let
          angle = turns ((0.75 - (0.25 * progressFactor)) + angleOffset)
        in
          ((40 * cos angle) + 40, (40 * sin angle) - 40)
      (Tile.North, Tile.West) ->
        let
          angle = turns (((0.25 * progressFactor) + 0.25) + angleOffset)
        in
          ((40 * cos angle) - 40, (40 * sin angle) - 40)
      (Tile.South, Tile.North) -> (0, -80 * progressFactor + 40)
      (Tile.South, Tile.East) ->
        let
          angle = turns ((0.75 + (0.25 * progressFactor)) + angleOffset)
        in
          ((40 * cos angle) + 40, (40 * sin angle) + 40)
      (Tile.South, Tile.West) ->
        let
          angle = turns ((0.25 - (0.25 * progressFactor)) + angleOffset)
        in
          ((40 * cos angle) - 40, (40 * sin angle) + 40)
      (Tile.East, Tile.West) -> (-80 * progressFactor + 40, 0)
      (Tile.East, Tile.North) ->
        let
          angle = turns ((0.5 + (0.25 * progressFactor)) + angleOffset)
        in
          ((40 * cos angle) + 40, (40 * sin angle) - 40)
      (Tile.East, Tile.South) ->
        let
          angle = turns ((1.0 - (0.25 * progressFactor)) + angleOffset)
        in
          ((40 * cos angle) + 40, (40 * sin angle) + 40)
      (Tile.West, Tile.East) -> (80 * progressFactor - 40, 0)
      (Tile.West, Tile.South) ->
        let
          angle = turns ((0.25 * progressFactor) + angleOffset)
        in
          ((40 * cos angle) + 40 - 80, (40 * sin angle) + 40)
      (Tile.West, Tile.North) ->
        let
          angle = turns ((0.5 - (0.25 * progressFactor)) + angleOffset)
        in
          ((40 * cos angle) + 40 - 80, (40 * sin angle) - 40)
      _ -> (0, 0)

calculateOffsets : Train.Model -> Float -> (Css.Mixin, Css.Mixin)
calculateOffsets train progressFactor =
  let
    { index, from, to } = train
    coordinates = indexToCoordinates index
    xOffset = toFloat (Tuple.first coordinates) * 80
    yOffset = toFloat (Tuple.second coordinates) * 80
    (xDelta, yDelta) = calculateDelta (from, to) progressFactor
    x = xOffset + xDelta
    y = yOffset + yDelta
  in
    (top (px y), left (px x))

nextIndex : Train.Model -> Maybe Int
nextIndex train =
  let
    max = boardWidth * boardHeight
    index = train.index
  in
    case train.to of
      Tile.North -> if index - boardWidth >= 0 then Just (index - boardWidth) else Nothing
      Tile.East -> if ((index + 1) % boardWidth) > 0 then Just (index + 1) else Nothing
      Tile.South -> if index + boardWidth < max then Just (index + boardWidth) else Nothing
      Tile.West -> if (index % boardWidth) > 0 then Just (index - 1) else Nothing
