module Coordinate exposing (..)

import List
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


dist : Coordinate -> Coordinate -> Float
dist coord1 coord2 =
    let
        a =
            first coord1 - first coord2

        b =
            second coord1 - second coord2
    in
    sqrt (toFloat (a ^ 2 + b ^ 2))


nearestCoord : List Coordinate -> Coordinate -> Coordinate
nearestCoord coords qcoord =
    let
        sortFunc =
            dist qcoord

        sorted =
            List.sortBy sortFunc coords

        nearest =
            List.head sorted
    in
    case nearest of
        Just coord ->
            coord

        Nothing ->
            qcoord
