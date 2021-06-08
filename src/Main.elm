module Main exposing (..)

import BoardSize exposing (BoardSize)
import Browser
import Browser.Events as E
import Coordinate exposing (Coordinate)
import GoBan exposing (draw, drawStone, newBoard)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as D
import Stone
import String
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width)



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { boardSize : BoardSize
    , mousePos : Coordinate
    }


type alias MousePos =
    { x : Int
    , y : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { boardSize = BoardSize.Nineteen
      , mousePos = ( 0, 0 )
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangeBoard BoardSize
    | MouseMove MousePos


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeBoard boardSize ->
            ( { model | boardSize = boardSize }, Cmd.none )

        MouseMove pos ->
            ( { model | mousePos = ( pos.x, pos.y ) }, Cmd.none )



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
                    ++ [ drawStone 0.75 goBan Stone.White model.mousePos ]
                )
            , button [ onClick (ChangeBoard BoardSize.Nineteen) ] [ text "19x19" ]
            , button [ onClick (ChangeBoard BoardSize.Thirteen) ] [ text "13x13" ]
            , button [ onClick (ChangeBoard BoardSize.Nine) ] [ text "9x9" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    E.onMouseMove (D.map MouseMove decodePos)


decodePos : D.Decoder MousePos
decodePos =
    D.map2 MousePos
        (D.field "pageX" D.int)
        (D.field "pageY" D.int)
