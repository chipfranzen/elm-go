module GoBan exposing (..)

import BoardSize exposing (BoardSize)
import Coordinate exposing (Coordinate)
import Html exposing (Html)
import List exposing (map, range)
import Stone exposing (Stone)
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
            BoardSize.toInt boardSize

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


draw : GoBan -> List (Svg msg)
draw goBan =
    let
        boardSize =
            BoardSize.toInt goBan.size

        rows =
            range 0 (boardSize - 1)
    in
    [ drawBoard goBan ]
        ++ List.map (drawHorizontal goBan) rows
        ++ List.map (drawVertical goBan) rows
        ++ drawStarPoints goBan


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
    List.map (Coordinate.translate start space) coords


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
    let
        mapFunc =
            List.map (drawStarPoint goBan)
    in
    case goBan.size of
        BoardSize.Nineteen ->
            mapFunc starPoints19

        BoardSize.Thirteen ->
            mapFunc starPoints13

        BoardSize.Nine ->
            mapFunc starPoints9


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


onGoBan : GoBan -> Coordinate -> Bool
onGoBan goBan pixel =
    let
        ( px, py ) =
            pixel

        pMin =
            goBan.viewboxBuffer

        pMax =
            pMin + goBan.width
    in
    (pMin <= px)
        && (px <= pMax)
        && (pMin <= py)
        && (py <= pMax)


drawStone : Float -> GoBan -> Stone -> Coordinate -> Html msg
drawStone alpha goBan stone pixel =
    let
        radius =
            String.fromInt (round (goBan.gridRes / 2))

        x =
            String.fromInt (first pixel)

        y =
            String.fromInt (second pixel)

        onBoard =
            onGoBan goBan pixel
    in
    case onBoard of
        False ->
            svg [] []

        True ->
            circle
                [ cx x
                , cy y
                , r radius
                , opacity (String.fromFloat alpha)
                , fill (Stone.toString stone)
                , stroke "black"
                , strokeWidth "1.2"
                ]
                []
