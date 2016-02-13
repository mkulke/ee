module Train (Model, init, size, view) where
import Graphics.Collage exposing (..)
import Color exposing (lightPurple)

-- MODEL


type alias Model = Int


size : Int
size = 50


init : Model
init =
  14


-- VIEW


view : Model -> Form
view model =
  square (toFloat size)
    |> filled lightPurple
