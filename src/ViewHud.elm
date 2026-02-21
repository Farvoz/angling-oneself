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
        [ css [ marginBottom (rem 0.5), fontSize (rem 0.9) ] ]
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
                , transform (translateY (pct -50))
                , displayFlex
                , justifyContent spaceBetween
                , alignItems center
                , padding2 zero (rem 1)
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
                [ text "Поискать новое место" ]
            ]

    else
        div
            [ css
                [ displayFlex
                , property "gap" "0.5rem"
                , position fixed
                , bottom zero
                , left zero
                , width (pct 100)
                , height (pct 100)
                , flexWrap wrap
                ]
            ]
            [ if List.length gameState.openTerrainCards < 5 && not (List.isEmpty gameState.conductingDeck) then
                button
                    [ onClick (GameMsg Pull)
                    , css [ btnStyle ]
                    ]
                    [ text "🎣" ]

              else
                text ""
            ]
