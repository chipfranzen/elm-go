module GoBan exposing (..)

import Html exposing (Html)
import List exposing (map, range)
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)


type alias GoBan =
    { size : BoardSize
    , width : Int
    , viewboxBuffer : Int
    , gridBuffer : Int
    , gridSize : Int
    , gridRes : Float
    }


newBoard : BoardSize -> Int -> Int -> GoBan
newBoard boardSize width viewboxBuffer =
    let
        boardSizeInt =
            toInt boardSize

        gridBuffer =
            (width - 50) // boardSizeInt

        gridSize =
            width - (2 * gridBuffer)

        gridRes =
            toFloat gridSize / (toFloat boardSizeInt - 1)
    in
    { size = boardSize
    , width = width
    , viewboxBuffer = viewboxBuffer
    , gridBuffer = gridBuffer
    , gridSize = gridSize
    , gridRes = gridRes
    }


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


type alias Coordinate =
    ( Int, Int )


draw : GoBan -> String -> Html msg
draw goBan viewBoxSize =
    let
        boardSize =
            toInt goBan.size

        rows =
            range 0 (boardSize - 1)
    in
    svg
        [ viewBox (String.join " " [ "0", "0", viewBoxSize, viewBoxSize ])
        , width viewBoxSize
        , height viewBoxSize
        ]
        ([ drawBoard goBan ]
            ++ List.map (drawHorizontal goBan) rows
            ++ List.map (drawVertical goBan) rows
            ++ drawStarPoints goBan
        )


drawBoard : GoBan -> Html msg
drawBoard goBan =
    rect
        [ x (String.fromInt goBan.viewboxBuffer)
        , y (String.fromInt goBan.viewboxBuffer)
        , width (String.fromInt goBan.width)
        , height (String.fromInt goBan.width)
        , fill "tan"
        , stroke "black"
        , strokeWidth "2"
        ]
        []


drawHorizontal : GoBan -> Int -> Html msg
drawHorizontal goBan rowNumber =
    let
        y =
            goBan.viewboxBuffer
                + goBan.gridBuffer
                + round (goBan.gridRes * toFloat rowNumber)
    in
    line
        [ x1 (String.fromInt (goBan.viewboxBuffer + goBan.gridBuffer))
        , y1 (String.fromInt y)
        , x2 (String.fromInt (goBan.viewboxBuffer + goBan.width - goBan.gridBuffer))
        , y2 (String.fromInt y)
        , stroke "black"
        , strokeWidth "1"
        , strokeLinecap "square"
        ]
        []


drawVertical : GoBan -> Int -> Html msg
drawVertical goBan colNumber =
    let
        x =
            goBan.viewboxBuffer
                + goBan.gridBuffer
                + round (goBan.gridRes * toFloat colNumber)
    in
    line
        [ x1 (String.fromInt x)
        , y1 (String.fromInt (goBan.viewboxBuffer + goBan.gridBuffer))
        , x2 (String.fromInt x)
        , y2 (String.fromInt (goBan.viewboxBuffer + goBan.width - goBan.gridBuffer))
        , stroke "black"
        , strokeWidth "1"
        , strokeLinecap "square"
        ]
        []


starPoints19 : List Coordinate
starPoints19 =
    let
        coords =
            [ ( 0, 0 )
            , ( 0, 1 )
            , ( 0, 2 )
            , ( 1, 0 )
            , ( 1, 1 )
            , ( 1, 2 )
            , ( 2, 0 )
            , ( 2, 1 )
            , ( 2, 2 )
            ]

        space =
            6

        start =
            ( 3, 3 )
    in
    List.map (coordTranslate start space) coords


starPoints13 : List Coordinate
starPoints13 =
    [ ( 3, 3 )
    , ( 3, 9 )
    , ( 9, 3 )
    , ( 9, 9 )
    , ( 6, 6 )
    ]


starPoints9 : List Coordinate
starPoints9 =
    [ ( 2, 2 )
    , ( 2, 6 )
    , ( 6, 2 )
    , ( 6, 6 )
    , ( 4, 4 )
    ]


drawStarPoint : GoBan -> Coordinate -> Html msg
drawStarPoint goBan gridCoord =
    let
        pixel =
            gridCoordToPixel goBan gridCoord

        radius =
            String.fromInt (round (goBan.gridRes / 6.0))
    in
    circle
        [ cx (String.fromInt (first pixel))
        , cy (String.fromInt (second pixel))
        , r radius
        ]
        []


drawStarPoints : GoBan -> List (Html msg)
drawStarPoints goBan =
    case goBan.size of
        Nineteen ->
            List.map (drawStarPoint goBan) starPoints19

        Thirteen ->
            List.map (drawStarPoint goBan) starPoints13

        Nine ->
            List.map (drawStarPoint goBan) starPoints9


gridCoordToPixel : GoBan -> Coordinate -> Coordinate
gridCoordToPixel goBan gridCoord =
    let
        ( gx, gy ) =
            gridCoord

        px =
            goBan.viewboxBuffer
                + goBan.gridBuffer
                + round (goBan.gridRes * toFloat gx)

        py =
            goBan.viewboxBuffer
                + goBan.gridBuffer
                + round (goBan.gridRes * toFloat gy)
    in
    ( px, py )


coordMul : Coordinate -> Int -> Coordinate
coordMul coord n =
    ( n * first coord, n * second coord )


coordAdd : Coordinate -> Coordinate -> Coordinate
coordAdd coord1 coord2 =
    ( first coord1 + first coord2
    , second coord1 + second coord2
    )


coordTranslate : Coordinate -> Int -> Coordinate -> Coordinate
coordTranslate locCoord scale inCoord =
    coordMul inCoord scale
        |> coordAdd locCoord
