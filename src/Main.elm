module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { step : Int
    }


init : Model
init =
    { step = 0 }


type Msg
    = MakeTurn
    | Restart


update : Msg -> Model -> Model
update msg model =
    case msg of
        MakeTurn ->
            { model | step = model.step + 1 }

        Restart ->
            { model | step = 0 }


view : Model -> Html Msg
view model =
    div [ style "padding" "2rem", style "font-family" "sans-serif" ]
        [ div [ style "margin-bottom" "1rem" ]
            [ text ("Шаг: " ++ String.fromInt model.step) ]
        , div [ style "display" "flex", style "gap" "0.5rem" ]
            [ button [ onClick MakeTurn ] [ text "Сделать ход" ]
            , button [ onClick Restart ] [ text "Начать заново" ]
            ]
        ]
