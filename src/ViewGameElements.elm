module ViewGameElements exposing (..)

import GameModel exposing (ConductingCard(..), GameMsg(..), Notch, TechniqueCard(..), TensionMode(..))
import Html.Styled exposing (Html, button, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Model exposing (Msg(..))
import Styles exposing (cardFishOutlineStyle, cardTechniqueStyle, cardTerrainStyle, notchStyle)
import Css exposing (..)

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
        (List.map viewTerrainCard cards)


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
                        let
                            sign = 
                                if tension.value > 0 then
                                    "+"
                                else 
                                    "-"
                        in
                        text <| sign ++ (String.fromInt <| abs tension.value)
                , viewNotch notch False
                ]

        FishOutlineCard { notches } ->
            div
                [ css [ cardFishOutlineStyle ] ]
                <| text "🐟" :: List.map (\notch -> viewNotch notch True) notches


techniqueCardLabel : TechniqueCard -> String
techniqueCardLabel card =
    case card of
        Observe ->
            "🔄 Наблюдение"
        Strike n ->
            "⚡ Подсечка " ++ String.fromInt n
        Maneuver n ->
            "⬇️ Манёвр " ++ (if n >= 0 then "+" else "") ++ String.fromInt n
        LoosenDrag n ->
            "🔓 Фрикцион " ++ String.fromInt n


viewTechniqueCard : TechniqueCard -> Html Msg
viewTechniqueCard card =
    div
        [ css [ cardTechniqueStyle ] ]
        [ text (techniqueCardLabel card) ]


viewOfferedTechniqueCards : List TechniqueCard -> Html Msg
viewOfferedTechniqueCards cards =
    div
        [ css
            [ displayFlex
            , flexDirection row
            , alignItems center
            , property "gap" "1rem"
            , flexShrink (num 0)
            ]
        ]
        (List.indexedMap
            (\index card ->
                button
                    [ onClick (GameMsg (SelectTechnique index))
                    , css [ cardTechniqueStyle, cursor pointer, borderStyle none ]
                    ]
                    [ text (techniqueCardLabel card) ]
            )
            cards
        )


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
                [ align (rem 0.1), left (px 2) ]

            2 ->
                [ align (rem 0.1), left (pct 50), transform (translateX (pct -50)) ]

            3 ->
                [ align (rem 0.1), right (px 2) ]

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
viewGameBoard terrainCards =
    div
        [ css
            [ flex (int 1)
            , displayFlex
            , flexDirection column
            , alignItems center
            , justifyContent flexEnd
            , marginBottom (rem 11)
            , marginTop (rem 3.5)
            , overflow auto
            ]
        ]
        [ viewOpenTerrainCards terrainCards ]
        
