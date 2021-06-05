module BoardSize exposing (..)

type BoardSize
    = Nine
    | Thirteen
    | Nineteen


toInt : BoardSize -> Int
toInt boardSize =
    case boardSize of
        Nine ->
            9

        Thirteen ->
            13

        Nineteen ->
            19
