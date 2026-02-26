module ViewGameElements exposing (..)

import Css exposing (..)
import Css.Animations exposing (keyframes)
import GameModel exposing (ConductingCard(..), GameMsg(..), Notch, TechniqueCard(..), TensionMode(..))
import Html.Styled exposing (Html, button, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Model exposing (Msg(..))
import Styles exposing (cardFishOutlineStyle, cardTechniqueStyle, cardTerrainStyle, notchStyle)


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
                        text ("=" ++ String.fromInt tension.value)

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
                [ css
                    ([ cardFishOutlineStyle ]
                        ++ [ property "position" "relative" ]
                    )
                ]
                ([ div
                    [ css
                        [ property "position" "absolute"
                        , property "top" "-50%"
                        , property "left" "-50%"
                        , property "width" "100px"
                        , property "height" "100px"
                        , property "pointer-events" "none"
                        , property "z-index" "1"
                        , property "border-radius" "50%"
                        , property "background"
                            "radial-gradient(circle, rgba(0,180,255,0.5) 10%, rgba(0,180,255,0.25) 50%, rgba(0,180,255,0.0) 100%)"
                        , property "animation" "wave-spread 0.9s cubic-bezier(.11,.53,.23,1.04) infinite"
                        , animationName waveSpread
                        ]
                    ]
                    []
                 , text "🐟"
                 ]
                    ++ List.map (\notch -> viewNotch notch True) notches
                )


waveSpread : Css.Animations.Keyframes {}
waveSpread =
    keyframes
        [ ( 0
          , [ Css.Animations.transform [ translate2 (px -50) (px -50) ]
            , Css.Animations.transform [ scale 0.7 ]
            , Css.Animations.opacity (num 0.8)
            ]
          )
        , ( 70
          , [ Css.Animations.transform [ translate2 (px -50) (px -50) ]
            , Css.Animations.transform [ scale 1.25 ]
            , Css.Animations.opacity (num 0.5)
            ]
          )
        , ( 100
          , [ Css.Animations.transform [ translate2 (px -50) (px -50) ]
            , Css.Animations.transform [ scale 1.6 ]
            , Css.Animations.opacity (num 0)
            ]
          )
        ]


techniqueCardLabel : TechniqueCard -> String
techniqueCardLabel card =
    case card of
        Observe ->
            "🔄 Наблюдение"

        Strike n ->
            "⚡ Подсечка " ++ String.fromInt n

        Maneuver n ->
            "⬇️ Манёвр "
                ++ (if n >= 0 then
                        "+"

                    else
                        ""
                   )
                ++ String.fromInt n

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
notchPositionStyles ( pos, _ ) isUpper =
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
        ( _, strength ) =
            notch
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
