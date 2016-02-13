module Train (Model, Action(Increment), init, update, size, view) where
import Graphics.Collage exposing (..)
import Color exposing (lightPurple)


-- MODEL


type alias Model = Int


size : Int
size = 50


init : Model
init =
  97


-- ACTION


type Action
  = Increment


-- UPDATE


update : Action -> Model -> Model
update action model =
  case action of
    Increment -> (model % 100) + 1


-- VIEW


view : Model -> Form
view model =
  square (toFloat size)
    |> filled lightPurple
