module ViewGameElements exposing (..)

import GameModel exposing (ConductingCard(..), NotchPosition(..))
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Model exposing (Msg)
import Styles exposing (cardBaitStyle, cardSlotStyle, cardTerrainStyle, notchStyle)
import Css exposing (displayFlex, flexDirection, alignItems, property, flexShrink, num, center)
import Css exposing (column, top, left, right, bottom, pct, transform, translateY, px)
import Css exposing (Style)
import Css exposing (flex)
import Css exposing (int)
import Css exposing (justifyContent)

viewOpenTerrainCards : List ConductingCard -> Html Msg
viewOpenTerrainCards cards =
    div
        [ css
            [ displayFlex
            , flexDirection column
            , alignItems center
            , property "gap" "4px"
            , flexShrink (num 0)
            ]
        ]
        (List.range 0 4
            |> List.map (\i -> viewCardSlot (List.drop i cards |> List.head))
        )


viewCardSlot : Maybe ConductingCard -> Html Msg
viewCardSlot maybeCard =
    case maybeCard of
        Just card ->
            viewTerrainCard card

        Nothing ->
            div [ css [ cardSlotStyle ] ] []


viewTerrainCard : ConductingCard -> Html Msg
viewTerrainCard card =
    case card of
        TerrainCard { tension, notchPosition, notchStrength } ->
            div
                [ css [ cardTerrainStyle ] ]
                [ text (String.fromInt tension)
                , viewNotch notchPosition notchStrength
                ]

        BaitCard _ ->
            div
                [ css [ cardBaitStyle ] ]
                [ text "Наживка" ]


notchPositionStyles : NotchPosition -> List Style
notchPositionStyles position =
    case position of
        TopLeft ->
            [ top (px 2), left (px 2) ]

        TopRight ->
            [ top (px 2), right (px 2) ]

        MiddleLeft ->
            [ top (pct 50), left (px 2), transform (translateY (pct -50)) ]

        MiddleRight ->
            [ top (pct 50), right (px 2), transform (translateY (pct -50)) ]

        BottomLeft ->
            [ bottom (px 2), left (px 2) ]

        BottomRight ->
            [ bottom (px 2), right (px 2) ]


viewNotch : NotchPosition -> Int -> Html Msg
viewNotch position strength =
    div
        [ css (notchStyle :: notchPositionStyles position) ]
        [ text (String.fromInt strength) ]


-- Layer 1: Game board (cards only)
viewGameBoard : List ConductingCard -> Html Msg
viewGameBoard cards =
    div
        [ css
            [ flex (int 1)
            , displayFlex
            , alignItems center
            , justifyContent center
            ]
        ]
        [ viewOpenTerrainCards cards ]
