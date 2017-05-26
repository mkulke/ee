module Board exposing (nextIndex, getNewDirections, tilesGenerator, calculateOffsets, calculateRotation, tilesOk)

import Tile exposing (Direction(..))
import Train
import Maybe
import Random exposing (generate)
import Css exposing (px, left, top, deg, transform, rotate)


boardWidth : Int
boardWidth =
    6


boardHeight : Int
boardHeight =
    6



-- the first tile should not point to either North or West


tilesOk : List Tile.Model -> Bool
tilesOk tiles =
    let
        pointsNorthOrWest =
            \tile ->
                case Tile.connections tile of
                    ( _, Tile.North ) ->
                        False

                    ( _, Tile.West ) ->
                        False

                    _ ->
                        True
    in
        case tiles of
            firstTile :: otherTiles ->
                pointsNorthOrWest firstTile

            [] ->
                False


tilesGenerator : Random.Generator (List Tile.Model)
tilesGenerator =
    Random.list (boardWidth * boardHeight) Tile.generator


indexToCoordinates : Int -> ( Int, Int )
indexToCoordinates index =
    ( index % boardWidth, index // boardHeight )


getNewDirections : Train.Model -> Tile.Model -> Maybe ( Tile.Direction, Tile.Direction )
getNewDirections train tile =
    let
        trainDirection =
            train.to

        rotateTwice =
            Tile.rotateDirection >> Tile.rotateDirection

        trainConnection =
            rotateTwice trainDirection

        ( oneEnd, otherEnd ) =
            Tile.connections tile
    in
        if trainConnection == oneEnd then
            Just ( oneEnd, otherEnd )
        else if trainConnection == otherEnd then
            Just ( otherEnd, oneEnd )
        else
            Nothing


calculateXTurn : Float -> Float -> Float
calculateXTurn delta factor =
    let
        angle =
            turns (factor / 4)
    in
        delta * cos angle


calculateYTurn : Float -> Float -> Float
calculateYTurn delta factor =
    let
        angle =
            turns (factor / 4)
    in
        delta * sin angle


calculateRotation : Train.Model -> Float -> Css.Mixin
calculateRotation model progressFactor =
    let
        degree =
            case ( model.from, model.to ) of
                ( North, South ) ->
                    0

                ( North, West ) ->
                    0 + round (90 * progressFactor)

                ( North, East ) ->
                    0 - round (90 * progressFactor)

                ( East, West ) ->
                    90

                ( East, North ) ->
                    90 + round (90 * progressFactor)

                ( East, South ) ->
                    90 - round (90 * progressFactor)

                ( West, East ) ->
                    270

                ( West, South ) ->
                    270 + round (90 * progressFactor)

                ( West, North ) ->
                    270 - round (90 * progressFactor)

                ( South, North ) ->
                    180

                ( South, East ) ->
                    180 + round (90 * progressFactor)

                ( South, West ) ->
                    180 - round (90 * progressFactor)

                _ ->
                    0
    in
        transform (rotate (deg degree))


type Direction
    = Clockwise
    | Counterclockwise


calculateDelta : ( Tile.Direction, Tile.Direction ) -> Float -> ( Float, Float )
calculateDelta movement progressFactor =
    let
        angleOffset =
            -0.25

        progress =
            0.25 * progressFactor

        angle =
            \( offset, direction ) ->
                case direction of
                    Clockwise ->
                        turns (offset + progress + angleOffset)

                    Counterclockwise ->
                        turns (offset - progress + angleOffset)

        turn =
            angle >> \angle_ -> ( 40 * cos angle_, 40 * sin angle_ )

        add =
            \( x1, y1 ) ( x2, y2 ) -> ( x1 + x2, y1 + y2 )
    in
        case movement of
            ( North, South ) ->
                ( 0, 80 * progressFactor - 40 )

            ( North, East ) ->
                turn ( 0.75, Counterclockwise ) |> add ( 40, -40 )

            ( North, West ) ->
                turn ( 0.25, Clockwise ) |> add ( -40, -40 )

            ( South, North ) ->
                ( 0, -80 * progressFactor + 40 )

            ( South, East ) ->
                turn ( 0.75, Clockwise ) |> add ( 40, 40 )

            ( South, West ) ->
                turn ( 0.25, Counterclockwise ) |> add ( -40, 40 )

            ( East, West ) ->
                ( -80 * progressFactor + 40, 0 )

            ( East, North ) ->
                turn ( 0.5, Clockwise ) |> add ( 40, -40 )

            ( East, South ) ->
                turn ( 1.0, Counterclockwise ) |> add ( 40, 40 )

            ( West, East ) ->
                ( 80 * progressFactor - 40, 0 )

            ( West, South ) ->
                turn ( 0.0, Clockwise ) |> add ( -40, 40 )

            ( West, North ) ->
                turn ( 0.5, Counterclockwise ) |> add ( -40, -40 )

            _ ->
                ( 0, 0 )


calculateOffsets : Train.Model -> Float -> ( Css.Mixin, Css.Mixin )
calculateOffsets train progressFactor =
    let
        { index, from, to } =
            train

        coordinates =
            indexToCoordinates index

        xOffset =
            toFloat (Tuple.first coordinates) * 80

        yOffset =
            toFloat (Tuple.second coordinates) * 80

        ( xDelta, yDelta ) =
            calculateDelta ( from, to ) progressFactor

        x =
            xOffset + xDelta

        y =
            yOffset + yDelta
    in
        ( top (px y), left (px x) )


nextIndex : Train.Model -> Maybe Int
nextIndex train =
    let
        max =
            boardWidth * boardHeight

        index =
            train.index
    in
        case train.to of
            Tile.North ->
                if index - boardWidth >= 0 then
                    Just (index - boardWidth)
                else
                    Nothing

            Tile.East ->
                if ((index + 1) % boardWidth) > 0 then
                    Just (index + 1)
                else
                    Nothing

            Tile.South ->
                if index + boardWidth < max then
                    Just (index + boardWidth)
                else
                    Nothing

            Tile.West ->
                if (index % boardWidth) > 0 then
                    Just (index - 1)
                else
                    Nothing
