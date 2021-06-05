-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


module Main exposing (..)

import BoardSize exposing (BoardSize)
import GoBan exposing (draw, newBoard)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    BoardSize


init : Model
init =
    BoardSize.Nineteen



-- UPDATE


type alias Msg =
    BoardSize


update : Msg -> Model -> Model
update msg model =
    msg



-- VIEW


view : Model -> Html Msg
view model =
    let
        goBan =
            newBoard model 500 50
    in
    div []
        [ div []
        [ draw goBan "600" ]
        , button [ onClick BoardSize.Nineteen ] [ text "19x19" ]
        , button [ onClick BoardSize.Thirteen ] [ text "13x13" ]
        , button [ onClick BoardSize.Nine ] [ text "9x9" ]
        ]
