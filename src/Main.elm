

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


module Main exposing (..)

import Browser
import List exposing (range, map)
import Tuple exposing (first, second)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = Int

type alias Coordinate = (Int, Int)


init : Model
init =
  0



-- UPDATE


type Msg
  = Increment
  | Decrement


update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1



-- VIEW

w : Int
w = 500

h : Int
h = 500

x_start : Int
x_start = 50

y_start : Int
y_start = 50

board_size : Int
board_size = 19

buffer: Int
buffer = 450 // board_size

grid_size : Int
grid_size = w - (2 * buffer)

res : Float
res = toFloat grid_size / (toFloat board_size - 1)

coords_19 = [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)]
space_19 = 6
start_19 = (3, 3)


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ Html.text "-" ]
    , div [] [ Html.text (String.fromInt model) ]
    , button [ onClick Increment ] [ Html.text "+" ],
    div []
    [ svg
    [ viewBox "0 0 1000 1000"
    , width "1000"
    , height "1000"
    ]
    (
    [
    rect
        [ x (String.fromInt x_start)
        , y (String.fromInt y_start)
        , width (String.fromInt w)
        , height (String.fromInt h)
        , fill "tan"
        , stroke "black"
        , strokeWidth "2"
        ]
        []
    ] ++
    List.map draw_horizontal (range 0 (board_size - 1 ))
    ++
    List.map draw_vertical (range 0 (board_size - 1))
    ++
    List.map draw_star_point star_points
    )
    ]]

draw_horizontal : Int -> Html Msg
draw_horizontal i =
  let
    y = y_start + buffer + round (res * toFloat i)
  in
  line
    [ x1 (String.fromInt (x_start + buffer))
    , y1 (String.fromInt y)
    , x2 (String.fromInt (x_start + w - buffer))
    , y2 (String.fromInt y)
    , stroke "black"
    , strokeWidth "1"
    ]
    []

draw_vertical : Int -> Html Msg
draw_vertical i =
  let
    x = x_start + buffer + round (res * toFloat i)
  in
  line
    [ x1 (String.fromInt x)
    , y1 (String.fromInt (y_start + buffer))
    , x2 (String.fromInt x)
    , y2 (String.fromInt (y_start + w - buffer - 1))
    , stroke "black"
    , strokeWidth "1"
    ]
    []

grid_coord_to_pixel : Coordinate -> Coordinate
grid_coord_to_pixel grid_coord =
  let
    (gx, gy) = grid_coord
    px = x_start + buffer + round (res * toFloat gx)
    py = y_start + buffer + round (res * toFloat gy)
  in
    (px, py)

pixel_to_grid_coord : Coordinate -> Coordinate
pixel_to_grid_coord pixel =
  let
    (px, py) = pixel
    gx = (px - x_start - buffer) // round res
    gy = (py - y_start - buffer) // round res
  in
    (gx, gy)

star_points : List Coordinate
star_points =
  let
    center = board_size // 2
    tengen = (center, center)
  in
    case board_size of
      19 ->
        List.map (tuple_translate start_19 space_19) coords_19

      _ ->
        []


tuple_mul : Coordinate -> Int -> Coordinate
tuple_mul coord n =
  (n * first(coord), n * second(coord))

tuple_add : Coordinate -> Coordinate -> Coordinate
tuple_add coord_1 coord_2 =
  (
    first(coord_1) + first(coord_2)
    , second(coord_1) + second(coord_2)
  )

tuple_translate : Coordinate -> Int -> Coordinate -> Coordinate
tuple_translate add_coord scale in_coord =
  tuple_mul in_coord scale
  |> tuple_add add_coord

draw_star_point : Coordinate -> Html Msg
draw_star_point grid_coord =
  let
    pixel = grid_coord_to_pixel grid_coord
  in
  circle
    [ cx (String.fromInt (first(pixel)))
    , cy (String.fromInt (second(pixel)))
    , r "4"
    ]
    []
