module Main exposing (..)

import BoardSize exposing (BoardSize)
import Browser
import Browser.Events as E
import Coordinate exposing (Coordinate)
import GoBan exposing (GoBan, draw, drawStone, newBoard, onGoBan, pixelToGridCoord)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as D
import Stone exposing (Stone)
import String
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (height, viewBox, width)



-- CONSTANTS


viewBoxSize : String
viewBoxSize =
    "600"


goBanWidth : Int
goBanWidth =
    500


viewBoxBuffer : Int
viewBoxBuffer =
    50



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { goBan : GoBan
    , stones : List ( Stone, Coordinate )
    , turn : Stone
    , mousePos : Coordinate
    }


type alias MousePos =
    { x : Int
    , y : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { goBan = newBoard BoardSize.Nineteen goBanWidth viewBoxBuffer
      , stones = []
      , turn = Stone.Black
      , mousePos = ( 0, 0 )
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangeBoard BoardSize
    | MouseMove MousePos
    | PlaceStone MousePos


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeBoard boardSize ->
            ( { model
                | goBan = newBoard boardSize goBanWidth viewBoxBuffer
                , stones = []
              }
            , Cmd.none
            )

        MouseMove pos ->
            ( { model | mousePos = ( pos.x, pos.y ) }, Cmd.none )

        PlaceStone pos ->
            placeStone model pos



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ svg
                [ viewBox (String.join " " [ "0", "0", viewBoxSize, viewBoxSize ])
                , width viewBoxSize
                , height viewBoxSize
                ]
                (draw model.goBan
                    ++ List.map (drawStone 1.0 model.goBan) model.stones
                    ++ drawGhostStone model
                )
            , button [ onClick (ChangeBoard BoardSize.Nineteen) ] [ text "19x19" ]
            , button [ onClick (ChangeBoard BoardSize.Thirteen) ] [ text "13x13" ]
            , button [ onClick (ChangeBoard BoardSize.Nine) ] [ text "9x9" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ E.onMouseMove (D.map MouseMove decodePos)
        , E.onClick (D.map PlaceStone decodePos)
        ]


decodePos : D.Decoder MousePos
decodePos =
    D.map2 MousePos
        (D.field "pageX" D.int)
        (D.field "pageY" D.int)


placeStone : Model -> MousePos -> ( Model, Cmd Msg )
placeStone model pos =
    let
        mouseCoord =
            ( pos.x, pos.y )

        onBan =
            onGoBan model.goBan mouseCoord
    in
    case onBan of
        True ->
            ( { model
                | stones = ( model.turn, pixelToGridCoord model.goBan ( pos.x, pos.y ) ) :: model.stones
                , turn = Stone.next model.turn
              }
            , Cmd.none
            )

        False ->
            ( model, Cmd.none )


drawGhostStone : Model -> List (Svg msg)
drawGhostStone model =
    let
        onBan =
            onGoBan model.goBan model.mousePos
    in
    case onBan of
        True ->
            [ drawStone 0.75 model.goBan ( model.turn, pixelToGridCoord model.goBan model.mousePos ) ]

        False ->
            []
