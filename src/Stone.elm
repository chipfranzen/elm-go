module Stone exposing (..)

import String


type Stone
    = Black
    | White


toString : Stone -> String
toString stone =
    case stone of
        Black ->
            "black"

        White ->
            "white"
