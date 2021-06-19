module StoneDict exposing (..)

import Coordinate exposing (Coordinate)
import Dict exposing (Dict)
import Stone exposing (Stone)


type alias StoneDict =
    Dict Coordinate Stone


isVacant : StoneDict -> Coordinate -> Bool
isVacant boardState location =
    case Dict.get location boardState of
        Nothing ->
            True

        Just stonecolor ->
            False
