module Main exposing (..)

import BoardSize exposing (BoardSize)
import Browser
import Browser.Events as E
import Coordinate exposing (Coordinate)
import Dict exposing (Dict)
import GoBan exposing (GoBan, draw, drawStone, newBoard, onGoBan, pixelToGridCoord)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as D
import MousePos exposing (MousePos)
import Stone exposing (Stone)
import StoneDict exposing (StoneDict)
import StoneGroup exposing (StoneGroup)
import String
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (height, viewBox, width)
import Tuple



-- CONSTANTS


viewBoxSize : String
viewBoxSize =
    "900"


goBanWidth : Int
goBanWidth =
    800


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
    , stones : StoneDict
    , stoneGroups : List StoneGroup
    , turn : Stone
    , mousePos : Coordinate
    , error : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { goBan = newBoard BoardSize.Nineteen goBanWidth viewBoxBuffer
      , stones = Dict.empty
      , stoneGroups = []
      , turn = Stone.Black
      , mousePos = ( 0, 0 )
      , error = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangeBoard BoardSize
    | MouseMove MousePos
    | BoardClick MousePos


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeBoard boardSize ->
            ( { model
                | goBan = newBoard boardSize goBanWidth viewBoxBuffer
                , stones = Dict.empty
                , stoneGroups = []
                , turn = Stone.Black
                , error = ""
              }
            , Cmd.none
            )

        MouseMove pos ->
            ( { model | mousePos = ( pos.x, pos.y ) }, Cmd.none )

        BoardClick pos ->
            boardClick model pos



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
                    ++ List.map (drawStone 1.0 model.goBan) (Dict.toList model.stones)
                    ++ drawGhostStone model
                )
            , text model.error
            , button [ onClick (ChangeBoard BoardSize.Nineteen) ] [ text "19x19" ]
            , button [ onClick (ChangeBoard BoardSize.Thirteen) ] [ text "13x13" ]
            , button [ onClick (ChangeBoard BoardSize.Nine) ] [ text "9x9" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ E.onMouseMove (D.map MouseMove MousePos.decodePos)
        , E.onClick (D.map BoardClick MousePos.decodePos)
        ]


boardClick : Model -> MousePos -> ( Model, Cmd Msg )
boardClick model pos =
    let
        mouseCoord =
            ( pos.x, pos.y )

        stoneCoord =
            pixelToGridCoord model.goBan mouseCoord

        onBan =
            onGoBan model.goBan mouseCoord
    in
    case onBan of
        True ->
            placeStone model stoneCoord

        False ->
            ( model, Cmd.none )


placeStone : Model -> Coordinate -> ( Model, Cmd Msg )
placeStone model stoneCoord =
    let
        legalMove =
            isLegal model stoneCoord
    in
    case legalMove of
        Legal ->
            updateStones model stoneCoord

        Illegal error ->
            ( { model | error = error }, Cmd.none )


updateStones : Model -> Coordinate -> ( Model, Cmd Msg )
updateStones model stoneCoord =
    let
        boardSizeInt =
            BoardSize.toInt model.goBan.boardSize

        stoneGroups =
            StoneGroup.updateGroups model.stoneGroups model.turn stoneCoord boardSizeInt

        liveGroups =
            StoneGroup.killGroups stoneGroups boardSizeInt

        stones =
            StoneGroup.toDict liveGroups
    in
    ( { model
        | stones = stones
        , stoneGroups = liveGroups
        , turn = Stone.next model.turn
        , error = ""
      }
    , Cmd.none
    )


isLegal : Model -> Coordinate -> MoveCheck
isLegal model stoneCoord =
    let
        vacantCoord =
            StoneDict.isVacant model.stones stoneCoord
    in
    case vacantCoord of
        True ->
            Legal

        False ->
            Illegal "Coordinate occupied! Play somewhere else."


type MoveCheck
    = Legal
    | Illegal String


drawGhostStone : Model -> List (Svg msg)
drawGhostStone model =
    let
        onBan =
            onGoBan model.goBan model.mousePos
    in
    case onBan of
        True ->
            [ drawStone 0.75 model.goBan ( pixelToGridCoord model.goBan model.mousePos, model.turn ) ]

        False ->
            []
