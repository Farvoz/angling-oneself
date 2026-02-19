module View exposing (view)

import Game exposing (ConductingCard(..), GamePhase(..), NotchPosition(..))
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Model exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    div [ style "padding" "2rem", style "font-family" "sans-serif" ]
        [ case model.gameState of
            Nothing ->
                div [] [ text "Загрузка..." ]

            Just gameState ->
                case gameState.phase of
                    Game.Won ->
                        div []
                            [ div [ style "font-size" "2rem", style "font-weight" "bold", style "color" "green", style "margin-bottom" "1rem" ]
                                [ text "Победа!" ]
                            , div [ style "margin-bottom" "1rem" ]
                                [ text
                                    ("Натяжение: "
                                        ++ String.fromInt gameState.lineTension
                                        ++ " | Рыб: "
                                        ++ String.fromInt gameState.caughtFish
                                        ++ " | Время: "
                                        ++ String.fromInt gameState.timeElapsed
                                    )
                                ]
                            , button
                                [ onClick Restart
                                , style "padding" "0.5rem 1rem"
                                , style "font-size" "1rem"
                                , style "cursor" "pointer"
                                ]
                                [ text "Начать заново" ]
                            ]

                    Game.Lost ->
                        div []
                            [ div [ style "font-size" "2rem", style "font-weight" "bold", style "color" "red", style "margin-bottom" "1rem" ]
                                [ text "Поражение" ]
                            , div [ style "margin-bottom" "1rem" ]
                                [ text
                                    ("Натяжение: "
                                        ++ String.fromInt gameState.lineTension
                                        ++ " | Рыб: "
                                        ++ String.fromInt gameState.caughtFish
                                        ++ " | Время: "
                                        ++ String.fromInt gameState.timeElapsed
                                    )
                                ]
                            , button
                                [ onClick Restart
                                , style "padding" "0.5rem 1rem"
                                , style "font-size" "1rem"
                                , style "cursor" "pointer"
                                ]
                                [ text "Начать заново" ]
                            ]

                    Game.Playing ->
                        div []
                            [ div [ style "margin-bottom" "1rem" ]
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
                            , div [ style "margin-bottom" "1rem" ]
                                [ viewOpenTerrainCards gameState.openTerrainCards
                                ]
                            , div [ style "margin-bottom" "1rem" ]
                                [ if List.length gameState.openTerrainCards < 5 && not (List.isEmpty gameState.conductingDeck) then
                                    button
                                        [ onClick Pull
                                        , style "padding" "0.5rem 1rem"
                                        , style "font-size" "1rem"
                                        , style "cursor" "pointer"
                                        , style "margin-right" "0.5rem"
                                        ]
                                        [ text "Тянуть" ]
                                  else
                                    text ""
                                , if List.length gameState.openTerrainCards == 5 then
                                    div []
                                        [ button
                                            [ onClick StayHere
                                            , style "padding" "0.5rem 1rem"
                                            , style "font-size" "1rem"
                                            , style "cursor" "pointer"
                                            , style "margin-right" "0.5rem"
                                            ]
                                            [ text "Остаться здесь" ]
                                        , button
                                            [ onClick SearchNewPlace
                                            , style "padding" "0.5rem 1rem"
                                            , style "font-size" "1rem"
                                            , style "cursor" "pointer"
                                            ]
                                            [ text "Поискать новое место" ]
                                        ]
                                  else
                                    text ""
                                ]
                            ]
        ]


viewOpenTerrainCards : List ConductingCard -> Html Msg
viewOpenTerrainCards cards =
    div
        [ style "display" "flex"
        , style "gap" "1rem"
        , style "flex-wrap" "wrap"
        , style "margin-bottom" "1rem"
        ]
        (List.map viewTerrainCard cards)


viewTerrainCard : ConductingCard -> Html Msg
viewTerrainCard card =
    case card of
        TerrainCard { tension, notchPosition, notchStrength } ->
            div
                [ style "width" "100px"
                , style "height" "140px"
                , style "border" "2px solid #333"
                , style "border-radius" "8px"
                , style "position" "relative"
                , style "display" "flex"
                , style "align-items" "center"
                , style "justify-content" "center"
                , style "background-color" "#f0f0f0"
                , style "font-size" "1.5rem"
                , style "font-weight" "bold"
                ]
                [ text (String.fromInt tension)
                , viewNotch notchPosition notchStrength
                ]

        BaitCard _ ->
            div
                [ style "width" "100px"
                , style "height" "140px"
                , style "border" "2px solid #333"
                , style "border-radius" "8px"
                , style "display" "flex"
                , style "align-items" "center"
                , style "justify-content" "center"
                , style "background-color" "#ffd700"
                ]
                [ text "Наживка" ]


viewNotch : NotchPosition -> Int -> Html Msg
viewNotch position strength =
    let
        ( verticalPos, horizontalPos ) =
            case position of
                TopLeft ->
                    ( "top", "left" )

                TopRight ->
                    ( "top", "right" )

                MiddleLeft ->
                    ( "50%", "left" )

                MiddleRight ->
                    ( "50%", "right" )

                BottomLeft ->
                    ( "bottom", "left" )

                BottomRight ->
                    ( "bottom", "right" )
    in
    div
        [ style "position" "absolute"
        , style verticalPos "5px"
        , style horizontalPos "5px"
        , style "width" "8px"
        , style "height" "8px"
        , style "background-color" "#333"
        , style "border-radius" "50%"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "font-size" "0.6rem"
        , style "color" "white"
        ]
        [ text (String.fromInt strength) ]
