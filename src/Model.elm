module Model exposing (Model, Msg(..), init, update)

import Game exposing (ConductingCard(..), GamePhase(..), GameState)
import Random


type alias Model =
    { gameState : Maybe GameState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gameState = Nothing
      }
    , Random.generate ShuffledDecks (Random.int Random.minInt Random.maxInt)
    )


type Msg
    = ShuffledDecks Int
    | Pull
    | StayHere
    | SearchNewPlace
    | Restart
    | ShuffledStayHere (List ConductingCard)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffledDecks seedValue ->
            let
                seed =
                    Random.initialSeed seedValue

                -- Используем seed для проводки, затем новый seed для приёмов
                ( shuffledConducting, seed1 ) =
                    Game.shuffleList seed Game.initialConductingDeck

                ( shuffledTechniques, _ ) =
                    Game.shuffleList seed1 Game.initialTechniquesDeck

                gameState =
                    { lineTension = 0
                    , caughtFish = 0
                    , timeElapsed = 0
                    , conductingDeck = shuffledConducting
                    , techniquesDeck = shuffledTechniques
                    , openTerrainCards = []
                    , discardedTerrainCards = []
                    , openTechniqueCards = []
                    , phase = Game.Playing
                    }
            in
            ( { model | gameState = Just gameState }, Cmd.none )

        Pull ->
            case model.gameState of
                Just gameState ->
                    if gameState.phase /= Game.Playing then
                        ( model, Cmd.none )
                    else
                        let
                            ( newDeck, maybeCard ) =
                                Game.drawCard gameState.conductingDeck
                        in
                        case maybeCard of
                            Nothing ->
                                -- Колода пуста - поражение
                                ( { model | gameState = Just { gameState | phase = Game.Lost } }, Cmd.none )

                            Just card ->
                                case card of
                                    TerrainCard { tension } ->
                                        let
                                            newOpenCards =
                                                gameState.openTerrainCards ++ [ card ]

                                            newTension =
                                                gameState.lineTension + tension
                                        in
                                        ( { model
                                            | gameState =
                                                Just
                                                    { gameState
                                                        | conductingDeck = newDeck
                                                        , openTerrainCards = newOpenCards
                                                        , lineTension = newTension
                                                    }
                                          }
                                        , Cmd.none
                                        )

                                    BaitCard _ ->
                                        let
                                            isVictory =
                                                Game.checkBaitVictory gameState.lineTension gameState.openTerrainCards card

                                            newPhase =
                                                if isVictory then
                                                    Game.Won
                                                else
                                                    Game.Playing
                                        in
                                        ( { model
                                            | gameState =
                                                Just
                                                    { gameState
                                                        | conductingDeck = newDeck
                                                        , discardedTerrainCards = gameState.discardedTerrainCards ++ [ card ]
                                                        , phase = newPhase
                                                    }
                                          }
                                        , Cmd.none
                                        )

                Nothing ->
                    ( model, Cmd.none )

        StayHere ->
            case model.gameState of
                Just gameState ->
                    if List.length gameState.openTerrainCards == 5 then
                        let
                            seed =
                                Random.initialSeed (gameState.timeElapsed + List.length gameState.openTerrainCards)

                            ( shuffledCards, _ ) =
                                Game.shuffleAndPrepend seed gameState.openTerrainCards gameState.conductingDeck
                        in
                        ( { model
                            | gameState =
                                Just
                                    { gameState
                                        | conductingDeck = shuffledCards
                                        , openTerrainCards = []
                                    }
                          }
                        , Cmd.none
                        )
                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SearchNewPlace ->
            case model.gameState of
                Just gameState ->
                    if List.length gameState.openTerrainCards == 5 then
                        ( { model
                            | gameState =
                                Just
                                    { gameState
                                        | discardedTerrainCards = gameState.discardedTerrainCards ++ gameState.openTerrainCards
                                        , openTerrainCards = []
                                    }
                          }
                        , Cmd.none
                        )
                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Restart ->
            ( { model | gameState = Nothing }
            , Random.generate ShuffledDecks (Random.int Random.minInt Random.maxInt)
            )

        ShuffledStayHere shuffledCards ->
            case model.gameState of
                Just gameState ->
                    ( { model
                        | gameState =
                            Just
                                { gameState
                                    | conductingDeck = shuffledCards ++ gameState.conductingDeck
                                    , openTerrainCards = []
                                }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )
