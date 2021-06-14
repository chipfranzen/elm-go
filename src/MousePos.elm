module MousePos exposing (..)

import Json.Decode as D


type alias MousePos =
    { x : Int
    , y : Int
    }


decodePos : D.Decoder MousePos
decodePos =
    D.map2 MousePos
        (D.field "pageX" D.int)
        (D.field "pageY" D.int)
