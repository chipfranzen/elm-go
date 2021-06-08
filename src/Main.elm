module Main exposing (..)

import BoardSize exposing (BoardSize)
import Browser
import Browser.Events as E
import Coordinate exposing (Coordinate)
import GoBan exposing (draw, drawStone, newBoard)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as D
import Stone exposing (Stone)
import String
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width)



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
    { boardSize : BoardSize
    , mousePos : Coordinate
    , stones : List ( Stone, Coordinate )
    , turn : Stone
    }


type alias MousePos =
    { x : Int
    , y : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { boardSize = BoardSize.Nineteen
      , mousePos = ( 0, 0 )
      , stones = []
      , turn = Stone.Black
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
            ( { model | boardSize = boardSize, stones = [] }, Cmd.none )

        MouseMove pos ->
            ( { model | mousePos = ( pos.x, pos.y ) }, Cmd.none )

        PlaceStone pos ->
            ( { model
                | stones = ( model.turn, ( pos.x, pos.y ) ) :: model.stones
                , turn = Stone.next model.turn
              }
            , Cmd.none
            )



-- VIEW


viewBoxSize : String
viewBoxSize =
    "600"


view : Model -> Html Msg
view model =
    let
        goBan =
            newBoard model.boardSize 500 50
    in
    div []
        [ div []
            [ svg
                [ viewBox (String.join " " [ "0", "0", viewBoxSize, viewBoxSize ])
                , width viewBoxSize
                , height viewBoxSize
                ]
                (draw goBan
                    ++ [ drawStone 0.75 goBan ( model.turn, model.mousePos ) ]
                    ++ List.map (drawStone 1.0 goBan) model.stones
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
