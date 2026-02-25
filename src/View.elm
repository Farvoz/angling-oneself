module View exposing (view)

import Css exposing (..)
import GameModel exposing (GameState)
import Html.Styled exposing (Html, button, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Model exposing (AppPhase(..), Model, Msg(..))
import Styles exposing (btnStyle, flexCenter, flexColumn, globalStyles, rootStyle)
import ViewGameElements exposing (viewGameBoard)
import ViewHud exposing (viewHudActions, viewHudDistanceTension, viewHudStats)


view : Model -> Html Msg
view model =
    div
        [ css [ rootStyle ] ]
        [ globalStyles
        , case model.appPhase of
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
    div [ css [ flexColumn, flexCenter ] ]
        [ div
            [ css
                [ fontSize (rem 2)
                , fontWeight bold
                , marginBottom (rem 1)
                ]
            ]
            [ text "Главное меню" ]
        , button
            [ onClick StartNewGame
            , css [ btnStyle ]
            ]
            [ text "Новая игра" ]
        ]


viewGame : GameState -> Html Msg
viewGame gameState =
    div
        [ css
            [ flexColumn
            , flex (int 1)
            , minHeight zero
            , justifyContent spaceBetween
            ]
        ]
        [ viewHudStats gameState
        , viewHudDistanceTension gameState
        , viewGameBoard gameState.openTerrainCards
        , viewHudActions gameState
        ]
