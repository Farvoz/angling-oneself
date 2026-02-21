module View exposing (view)

import GameModel exposing (ConductingCard(..), GameMsg(..), GamePhase(..), NotchPosition(..))
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Model exposing (Model, Msg(..), AppPhase(..))
import GameView exposing (..)

view : Model -> Html Msg
view model =
    div [ style "padding" "2rem", style "font-family" "sans-serif" ]
        [ case model.appPhase of
            MainMenu ->
                viewMainMenu

            InGame ->
                case model.gameState of
                    Nothing ->
                        div [] [ text "Загрузка..." ]

                    Just gameState ->
                        viewGame gameState
        ]


viewMainMenu : Html Msg
viewMainMenu =
    div []
        [ div [ style "font-size" "2rem", style "font-weight" "bold", style "margin-bottom" "1rem" ]
            [ text "Главное меню" ]
        , button
            [ onClick StartNewGame
            , style "padding" "0.75rem 1.25rem"
            , style "min-height" "44px"
            , style "font-size" "1rem"
            , style "cursor" "pointer"
            ]
            [ text "Новая игра" ]
        ]

