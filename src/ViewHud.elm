module ViewHud exposing (..)

import GameModel exposing (..)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Model exposing (Msg)
import Styles exposing (btnStyle)
import Css exposing (..)
import Html.Styled exposing (button)
import Model exposing (Msg(..))


-- Layer 2: HUD components (stats and action buttons)
viewHudStats : GameState -> Html Msg
viewHudStats gameState =
    div
        [ css 
            [ position fixed
            , marginBottom (rem 0.5)
            , fontSize (rem 0.9) ] 
            ]
        [ text
            ("Натяжение: "
                ++ String.fromInt gameState.lineTension
                ++ " | Рыб: "
                ++ String.fromInt gameState.caughtFish
                ++ " | Время: "
                ++ String.fromInt gameState.timeElapsed
                ++ " | Карт в колоде: "
                ++ String.fromInt (List.length gameState.conductingDeck)
            )
        ]


viewHudActions : GameState -> Html Msg
viewHudActions gameState =
    if List.length gameState.openTerrainCards == 5 then
        div
            [ css
                [ position fixed
                , top (pct 50)
                , left zero
                , right zero
                , padding2 zero (rem 0.75)
                , transform (translateY (pct -50))
                , displayFlex
                , justifyContent spaceBetween
                , alignItems center
                ]
            ]
            [ button
                [ onClick (GameMsg StayHere)
                , css [ btnStyle ]
                ]
                [ text "Остаться здесь" ]
            , button
                [ onClick (GameMsg SearchNewPlace)
                , css [ btnStyle ] 
                ]
                [ text "Новое место" ]
            ]

    else
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
            [ if List.length gameState.openTerrainCards < 5 && not (List.isEmpty gameState.conductingDeck) then
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
