module ViewHud exposing (..)

import GameModel exposing (..)
import Html.Styled exposing (Html, button, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Styles exposing (btnStyle)
import Css exposing (..)
import Model exposing (Msg(..))
import ThemeTokens exposing (surfaceAccent, textOnAccent)


phaseLabel : GamePhase -> String
phaseLabel phase =
    case phase of
        ReadyToCast ->
            "Готов к забросу"
        Conducting ->
            "Проводка"
        Fighting ->
            "Вываживание"


phaseChangeLabel : PhaseChange -> String
phaseChangeLabel change =
    phaseLabel change.from
        ++ " → "
        ++ phaseLabel change.to
        ++ ": "
        ++ change.reason


-- Layer 2: HUD components (stats and action buttons)
viewHudDistanceTension : GameState -> Html Msg
viewHudDistanceTension gameState =
    div
        [ css
            [ position fixed
            , bottom (rem 12)
            , left (rem 7.5)
            , displayFlex
            , justifyContent spaceBetween
            , property "gap" "4rem" 
            ]
        ]
        [ div [] [ text ("📏 " ++ String.fromInt gameState.distance) ]
        , div [] [ text ("⚡ " ++ String.fromInt gameState.lineTension) ]
        ]


viewHudStats : GameState -> Html Msg
viewHudStats gameState =
    div
        [ css 
            [ position fixed
            , marginBottom (rem 0.5)
            , fontSize (rem 0.9)
            , displayFlex
            , flexDirection column
            , property "gap" "0.25rem"
            ]
            ]
        [ div []
            [ text
                ("🌘: "
                    ++ phaseLabel gameState.phase
                    ++ " | 🐟: "
                    ++ String.fromInt gameState.caughtFish
                    ++ " | ⏳: "
                    ++ String.fromInt gameState.timeElapsed
                    ++ " | 🎴: "
                    ++ String.fromInt (List.length gameState.conductingDeck)
                )
            ]
        , div
            [ css
                [ fontSize (rem 0.85)
                , marginTop (rem 1)
                , property "max-width" "calc(100vw - 1rem)"
                ]
            ]
            (gameState.phaseChanges
                |> List.take 1
                |> List.map (\change -> div [] [ text (phaseChangeLabel change) ])
            )
        ]


viewHudActions : GameState -> Html Msg
viewHudActions gameState =
    case gameState.phase of
        ReadyToCast ->
            div
                [ css
                    [ position fixed
                    , left zero
                    , bottom zero
                    , width (pct 100)
                    , padding (rem 0.5)
                    , displayFlex
                    , flexDirection column
                    , alignItems center
                    , property "gap" "0.5rem"
                    ]
                ]
                [ div
                    [ css
                        [ displayFlex
                        , flexWrap wrap
                        , justifyContent center
                        , property "gap" "0.5rem"
                        ]
                    ]
                    (List.range 2 10
                        |> List.map
                            (\n ->
                                let
                                    isSelected =
                                        gameState.selectedDistance == Just n
                                in
                                button
                                    [ onClick (GameMsg (SelectDistance n))
                                    , css
                                        [ btnStyle
                                        , if isSelected then
                                            batch [ backgroundColor surfaceAccent, color textOnAccent ]
                                          else
                                            batch []
                                        ]
                                    ]
                                    [ text (String.fromInt n) ]
                            )
                    )
                , div
                    [ css
                        [ displayFlex
                        , justifyContent center
                        , property "gap" "0.5rem"
                        ]
                    ]
                    [ button
                        [ onClick (GameMsg Cast)
                        , css [ btnStyle ]
                        ]
                        [ text "🎣 Забросить" ]
                    , button
                        [ onClick (GameMsg SearchNewPlace)
                        , css [ btnStyle ]
                        ]
                        [ text "🔍 Искать" ]
                    ]
                ]

        Conducting ->
            viewHudActionsConductingOrFighting gameState

        Fighting ->
            viewHudActionsConductingOrFighting gameState


viewHudActionsConductingOrFighting : GameState -> Html Msg
viewHudActionsConductingOrFighting gameState =
    div
        [ css
            [ displayFlex
            , position fixed
            , left zero
            , bottom zero
            , width (pct 100)
            , height (rem 11)
            , justifyContent center
            ]
        ]
        [ if not (List.isEmpty gameState.conductingDeck) then
            button
                [ onClick (GameMsg Pull)
                , css
                    [ btnStyle
                    , width (rem 3)
                    , height (rem 3)
                    ]
                ]
                [ text "🎣" ]

          else
            text ""
        ]
