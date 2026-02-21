module ViewGameElements exposing (..)

import GameModel exposing (ConductingCard(..), Notch, TensionMode(..))
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Model exposing (Msg)
import Styles exposing (cardBaitStyle, cardSlotStyle, cardTerrainStyle, notchStyle)
import Css exposing (..)
import Char exposing (isUpper)

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
        TerrainCard { tension, notch } ->
            div
                [ css [ cardTerrainStyle ] ]
                [ case tension.mode of
                    TensionSet ->
                        text ("=" ++ (String.fromInt tension.value))
                    TensionChange ->
                        text (String.fromInt tension.value)
                , viewNotch notch False
                ]

        BaitCard { notches } ->
            div
                [ css [ cardBaitStyle ] ]
                <| text "⚜️" :: List.map (\notch -> viewNotch notch True) notches 


notchPositionStyles : Notch -> Bool -> List Style
notchPositionStyles (pos, _) isUpper =
    let
        align = 
            if isUpper then
                top
            else 
                bottom
    in
        case pos of
            1 ->
                [ align (rem 0.25), left (px 2) ]

            2 ->
                [ align (rem 0.25), left (pct 50), transform (translateX (pct -50)) ]

            3 ->
                [ align (rem 0.25), right (px 2) ]

            _ ->
                []


viewNotch : Notch -> Bool -> Html Msg
viewNotch notch isUpper =
    let
        (_, strength) = notch
    in
        div
            [ css (notchStyle :: notchPositionStyles notch isUpper) ]
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
