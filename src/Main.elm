module Main exposing (main)

import Board
import Browser exposing (..)
import Browser.Events exposing (onAnimationFrameDelta)
import Html.Styled exposing (Html, div, toUnstyled)
import Html.Styled.Attributes exposing (class)
import List.Extra exposing (getAt, groupsOf, setAt)
import Maybe exposing (andThen, withDefault)
import Random exposing (generate)
import Tile
import Train


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view >> toUnstyled
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { tiles : List Tile.Model
    , train : Maybe Train.Model
    }


generateTiles : Cmd Msg
generateTiles =
    generate NewRandom Board.tilesGenerator


init : flags -> ( Model, Cmd Msg )
init flags =
    ( Model [] Nothing
    , generateTiles
    )


type alias Index =
    Int



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameDelta Tick



-- MSG


type Msg
    = Tile Index Tile.Msg
    | Tick Float
    | NewRandom (List Tile.Model)


updateTiles : Tile.Msg -> Index -> List Tile.Model -> List Tile.Model
updateTiles msg index tiles =
    let
        tileUpdate =
            Tile.update msg

        tile =
            getAt index tiles
    in
    case tile of
        Just t ->
            setAt index (tileUpdate t) tiles

        Nothing ->
            tiles


initTrain : List Tile.Model -> Int -> Maybe Train.Model
initTrain tiles index =
    let
        tilesTail =
            List.drop index tiles
    in
    case tilesTail of
        firstTile :: _ ->
            Just (Train.init index firstTile)

        [] ->
            Nothing


moveTrain : List Tile.Model -> Train.Model -> Maybe Train.Model
moveTrain tiles train =
    let
        assertIdle =
            \tile ->
                if Tile.idle tile then
                    Just tile

                else
                    Nothing

        setIndex =
            \index -> { train | index = index }

        setDirection =
            \newTrain ->
                getAt newTrain.index tiles
                    |> Maybe.andThen assertIdle
                    |> Maybe.andThen (Board.getNewDirections newTrain)
                    |> Maybe.map (\( from, to ) -> { newTrain | from = from, to = to })
    in
    Board.nextIndex train
        |> Maybe.map setIndex
        |> Maybe.andThen setDirection


updateTrainTick : Float -> Model -> Train.Model -> Train.Model
updateTrainTick diff model train =
    let
        progressedTrain =
            Train.updateProgress diff train
    in
    case progressedTrain.progress of
        Train.ProgressingSince _ ->
            progressedTrain

        Train.Done ->
            moveTrain model.tiles progressedTrain
                |> withDefault train


updateTick : Float -> Model -> Model
updateTick diff model =
    let
        currentIndex =
            Maybe.map (\train -> train.index) model.train

        nextIndex =
            model.train |> Maybe.andThen Board.nextIndex

        updateOccupancy =
            \referenceIndex index tile ->
                case referenceIndex of
                    Just i ->
                        if index == i then
                            Tile.setOccupancy Tile.Blocked tile

                        else
                            Tile.setOccupancy Tile.Vacant tile

                    Nothing ->
                        tile

        updatedTiles =
            model.tiles
                |> List.indexedMap (updateOccupancy currentIndex)
                |> List.indexedMap (updateOccupancy nextIndex)
                |> List.map updateTile

        updateTrain =
            updateTrainTick diff model

        updateTile =
            Tile.updateTick diff
    in
    { model
        | tiles = updatedTiles
        , train = Maybe.map updateTrain model.train
    }


populateBoard : Model -> List Tile.Model -> Model
populateBoard model tiles =
    { model | tiles = tiles, train = initTrain tiles 0 }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tile index tileMsg ->
            ( { model | tiles = updateTiles tileMsg index model.tiles }
            , Cmd.none
            )

        Tick diff ->
            ( updateTick diff model
            , Cmd.none
            )

        NewRandom tiles ->
            if Board.tilesOk tiles then
                ( populateBoard model tiles
                , Cmd.none
                )

            else
                ( model
                , generateTiles
                )



-- VIEW


view : Model -> Html Msg
view model =
    let
        tileView =
            -- \index tile -> Html.map (Tile index) (Tile.debugView tile index)
            \index tile -> Html.Styled.map (Tile index) (Tile.view tile)

        trainView =
            \train -> Train.view Board.calculateOffsets Board.calculateRotation train

        tilesView =
            List.indexedMap tileView model.tiles

        tileRows =
            groupsOf 6 tilesView |> List.map (div [])

        divs =
            case model.train of
                Nothing ->
                    tileRows

                Just train ->
                    trainView train :: tileRows
    in
    div [ class "grid" ] divs
