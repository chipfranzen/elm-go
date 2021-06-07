module Coordinate exposing (..)

import Tuple exposing (first, second)

type alias Coordinate =
    ( Int, Int )

mul : Coordinate -> Int -> Coordinate
mul coord n =
    ( n * first coord, n * second coord )


add : Coordinate -> Coordinate -> Coordinate
add coord1 coord2 =
    ( first coord1 + first coord2
    , second coord1 + second coord2
    )


translate : Coordinate -> Int -> Coordinate -> Coordinate
translate locCoord scale inCoord =
    mul inCoord scale
        |> add locCoord
