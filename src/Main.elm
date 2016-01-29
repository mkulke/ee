import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)


type Orientation = North
                 | East
                 | South
                 | West


type Kind = Left
          | Right
          | Straight


type alias Tile =
  { kind : Kind
  , orientation : Orientation
  }


main : Element
main =
  let offset = (toFloat tileSize) / 2
  in
    collage boardSize boardSize
      [ move (-offset, offset) (tile (Tile Right West))
      , move (-offset, -offset) (tile (Tile Straight West))
      , move (offset, offset) (tile (Tile Left East))
      , move (offset, -offset) (tile (Tile Left West))
      ]


tileSize : Int
tileSize = 50


boardSize : Int
boardSize = tileSize * 2


tile : Tile -> Form
tile tile =
  let path {kind} = case kind of
        Left -> "/img/left.png"
        Right -> "/img/right.png"
        Straight -> "/img/straight.png"
      rotation {orientation} = case orientation of
        North -> 0
        East -> 90
        South -> 180
        West -> 270
      tileRotate = rotate (rotation tile |> degrees)
  in
    path tile
      |> image tileSize tileSize
      |> toForm
      |> tileRotate
