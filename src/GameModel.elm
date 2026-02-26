module GameModel exposing
    ( Bait
    , ConductingCard(..)
    , GameMsg(..)
    , GameState
    , HookPositionCondition(..)
    , Notch
    , TechniqueCard(..)
    , TensionChangeCondition(..)
    , TensionMatchCondition(..)
    , TensionMode(..)
    , applyTechniqueEffect
    , checkBaitVictory
    , updateGame
    )

import Array
import GameInventory.Deck exposing (..)
import GameModel.Phases exposing (..)
import Random



-- Типы для колоды проводки
-- (Позиция, Сила)


type alias Notch =
    ( Int, Int )


type TensionMode
    = TensionSet
    | TensionChange


type alias Tension =
    { mode : TensionMode
    , value : Int
    }


type ConductingCard
    = TerrainCard { tension : Tension, notch : Notch }
    | FishOutlineCard { notches : List Notch }



-- Типы для колоды приёмов


type TechniqueCard
    = Observe
    | Strike Int
    | Maneuver Int
    | LoosenDrag Int



-- Типы условий наживки


type HookPositionCondition
    = NotchNotConsidered -- не учитывается положение засечки
    | NotchConsidered -- учитывается положение засечки


type TensionMatchCondition
    = StrictlyEqual -- строго равно
    | AtLeastOrMore Int -- такое или на Х больше (X — параметр)


type TensionChangeCondition
    = BreaksOnSuddenPlus2 -- рвётся при резком +2
    | NoEffect -- не влияет


type alias Bait =
    { maxTension : Int
    , hookPositionCondition : HookPositionCondition
    , tensionMatchCondition : TensionMatchCondition
    , tensionChangeCondition : TensionChangeCondition
    }



-- Игровое состояние


type alias GameState =
    { lineTension : Int
    , distance : Int
    , caughtFish : Int
    , timeElapsed : Int
    , conductingDeck : List ConductingCard
    , techniquesDeck : List TechniqueCard
    , openTerrainCards : List ConductingCard
    , discardedTerrainCards : List ConductingCard
    , openTechniqueCards : List TechniqueCard
    , offeredTechniqueCards : List TechniqueCard
    , phase : GamePhase
    , phaseChanges : List PhaseChange
    , seed : Random.Seed
    , selectedDistance : Maybe Int
    , availableBaits : List Bait
    , equippedBaitIndex : Maybe Int
    }



-- Игровые сообщения


type GameMsg
    = Pull
    | UseTechnique
    | SelectTechnique Int
    | SelectDistance Int
    | SelectBait Int
    | Cast
    | SearchNewPlace


resetForCast : GameState -> GameState
resetForCast gameState =
    let
        ( shuffledCards, seed1 ) =
            shuffleAndPrepend gameState.seed gameState.openTerrainCards gameState.conductingDeck
    in
    { gameState
        | conductingDeck = shuffledCards
        , openTerrainCards = []
        , lineTension = 0
        , seed = seed1
    }


toReadyToCast : String -> GameState -> GameState
toReadyToCast reason gameState =
    let
        phasesResult =
            transitionPhase ( gameState.phase, ReadyToCast ) reason gameState.phaseChanges
    in
    { gameState | phase = phasesResult.phase, phaseChanges = phasesResult.phaseChanges }


updateTenstion : ConductingCard -> Int -> Int
updateTenstion card currentTension =
    case card of
        TerrainCard { tension } ->
            case tension.mode of
                TensionSet ->
                    tension.value

                TensionChange ->
                    max 0 (currentTension + tension.value)

        FishOutlineCard _ ->
            currentTension


getEquippedBait : GameState -> Maybe Bait
getEquippedBait gameState =
    gameState.equippedBaitIndex
        |> Maybe.andThen (\i -> List.head (List.drop i gameState.availableBaits))


loseBait : GameState -> GameState
loseBait gameState =
    case gameState.equippedBaitIndex of
        Just i ->
            if i >= 0 && i < List.length gameState.availableBaits then
                let
                    newBaits =
                        removeAt i gameState.availableBaits
                in
                { gameState
                    | availableBaits = newBaits
                    , equippedBaitIndex = Nothing
                }

            else
                { gameState | equippedBaitIndex = Nothing }

        Nothing ->
            gameState


updateGame : GameMsg -> GameState -> GameState
updateGame msg gameState =
    case msg of
        SelectDistance n ->
            if gameState.phase == ReadyToCast && n >= 2 && n <= 10 then
                { gameState | selectedDistance = Just n }

            else
                gameState

        SelectBait index ->
            if gameState.phase == ReadyToCast && index >= 0 && index < List.length gameState.availableBaits then
                { gameState | equippedBaitIndex = Just index }

            else
                gameState

        Pull ->
            if gameState.phase /= Conducting && gameState.phase /= Fighting then
                gameState

            else
                let
                    ( newDeck, maybeCard ) =
                        drawCard gameState.conductingDeck
                in
                case maybeCard of
                    Nothing ->
                        toReadyToCast "колода закончилась" gameState

                    Just card ->
                        let
                            newTension =
                                updateTenstion card gameState.lineTension

                            newOpenCards =
                                gameState.openTerrainCards ++ [ card ]

                            newDistance =
                                gameState.distance - newTension

                            baseState =
                                { gameState
                                    | conductingDeck = newDeck
                                    , openTerrainCards = newOpenCards
                                    , lineTension = newTension
                                }

                            maybeBait =
                                getEquippedBait gameState

                            baitLost =
                                case maybeBait of
                                    Nothing ->
                                        False

                                    Just bait ->
                                        newTension
                                            > bait.maxTension
                                            || (bait.tensionChangeCondition
                                                    == BreaksOnSuddenPlus2
                                                    && (newTension - gameState.lineTension)
                                                    >= 2
                                               )
                        in
                        if baitLost then
                            baseState
                                |> loseBait
                                |> toReadyToCast "наживка оборвалась"

                        else if newTension <= 0 then
                            if gameState.phase == Fighting then
                                toReadyToCast "натяжение упало до 0" baseState

                            else
                                { baseState | distance = newDistance }

                        else
                            case gameState.phase of
                                Conducting ->
                                    case card of
                                        FishOutlineCard _ ->
                                            if checkBaitVictory (getEquippedBait gameState) newTension gameState.openTerrainCards card then
                                                let
                                                    { phase, phaseChanges } =
                                                        transitionPhase ( baseState.phase, Fighting ) "клюнуло на наживку" baseState.phaseChanges
                                                in
                                                { baseState
                                                    | distance = newDistance
                                                    , phase = phase
                                                    , phaseChanges = phaseChanges
                                                }

                                            else if newDistance <= 0 then
                                                { baseState
                                                    | distance = newDistance
                                                    , timeElapsed = gameState.timeElapsed + 1
                                                }
                                                    |> toReadyToCast "проводка завершена: дистанция исчерпана"

                                            else
                                                { baseState | distance = newDistance }

                                        TerrainCard _ ->
                                            if newDistance <= 0 then
                                                { baseState
                                                    | distance = newDistance
                                                    , timeElapsed = gameState.timeElapsed + 1
                                                }
                                                    |> toReadyToCast "проводка завершена: дистанция исчерпана"

                                            else
                                                { baseState | distance = newDistance }

                                Fighting ->
                                    if newDistance <= 0 then
                                        { baseState
                                            | distance = newDistance
                                            , caughtFish = gameState.caughtFish + 1
                                            , timeElapsed = gameState.timeElapsed + 1
                                        }
                                            |> toReadyToCast "рыба вытащена"

                                    else
                                        { baseState | distance = newDistance }

                                _ ->
                                    baseState

        UseTechnique ->
            case gameState.phase of
                Conducting ->
                    if List.isEmpty gameState.techniquesDeck then
                        gameState

                    else
                        let
                            ( newDeck, drawn ) =
                                drawTechniqueCards 2 gameState.techniquesDeck

                            phasesResult =
                                GameModel.Phases.transitionPhase ( gameState.phase, TechniqueChoice Conducting ) "приём" gameState.phaseChanges
                        in
                        { gameState
                            | techniquesDeck = newDeck
                            , offeredTechniqueCards = drawn
                            , phase = phasesResult.phase
                            , phaseChanges = phasesResult.phaseChanges
                        }

                Fighting ->
                    if List.isEmpty gameState.techniquesDeck then
                        gameState

                    else
                        let
                            ( newDeck, drawn ) =
                                drawTechniqueCards 2 gameState.techniquesDeck

                            phasesResult =
                                GameModel.Phases.transitionPhase ( gameState.phase, TechniqueChoice Conducting ) "приём" gameState.phaseChanges
                        in
                        { gameState
                            | techniquesDeck = newDeck
                            , offeredTechniqueCards = drawn
                            , phase = phasesResult.phase
                            , phaseChanges = phasesResult.phaseChanges
                        }

                _ ->
                    gameState

        SelectTechnique index ->
            case gameState.phase of
                TechniqueChoice returnPhase ->
                    case ( List.head (List.drop index gameState.offeredTechniqueCards), index >= 0, index < List.length gameState.offeredTechniqueCards ) of
                        ( Just card, True, True ) ->
                            let
                                unselected =
                                    List.indexedMap Tuple.pair gameState.offeredTechniqueCards
                                        |> List.filter (\( i, _ ) -> i /= index)
                                        |> List.map Tuple.second

                                newDeck =
                                    unselected ++ gameState.techniquesDeck

                                phaseResult =
                                    transitionPhase ( gameState.phase, returnPhase ) "приём использован" gameState.phaseChanges
                            in
                            gameState
                                |> applyTechniqueEffect card
                                |> (\s ->
                                        { s
                                            | offeredTechniqueCards = []
                                            , techniquesDeck = newDeck
                                            , phase = phaseResult.phase
                                            , phaseChanges = phaseResult.phaseChanges
                                        }
                                   )

                        _ ->
                            gameState

                _ ->
                    gameState

        Cast ->
            case gameState.selectedDistance of
                Just n ->
                    if gameState.phase == ReadyToCast && n >= 2 && n <= 10 && gameState.equippedBaitIndex /= Nothing && not (List.isEmpty gameState.availableBaits) then
                        let
                            phaseResult =
                                transitionPhase ( gameState.phase, Conducting ) ("выбрана дистанция " ++ String.fromInt n) gameState.phaseChanges
                        in
                        gameState
                            |> resetForCast
                            |> (\s -> { s | distance = n, selectedDistance = Nothing, phase = phaseResult.phase, phaseChanges = phaseResult.phaseChanges })

                    else
                        gameState

                Nothing ->
                    gameState

        SearchNewPlace ->
            case gameState.selectedDistance of
                Just n ->
                    if gameState.phase == ReadyToCast && n >= 2 && n <= 10 then
                        let
                            ( shuffledDeck, seed1 ) =
                                shuffleAndPrepend gameState.seed gameState.openTerrainCards gameState.conductingDeck

                            newDeck =
                                moveTopToBottom n shuffledDeck
                        in
                        { gameState
                            | conductingDeck = newDeck
                            , openTerrainCards = []
                            , seed = seed1
                            , timeElapsed = gameState.timeElapsed + n
                            , selectedDistance = Nothing
                        }

                    else
                        gameState

                Nothing ->
                    gameState



-- Применить эффект приёма к состоянию игры


applyTechniqueEffect : TechniqueCard -> GameState -> GameState
applyTechniqueEffect card gameState =
    case card of
        Strike n ->
            { gameState | lineTension = max 0 (gameState.lineTension + n) }

        Maneuver n ->
            { gameState | lineTension = max 0 (gameState.lineTension - n) }

        LoosenDrag n ->
            { gameState | lineTension = 1 }

        Observe ->
            gameState



-- Проверить победу при раскрытии карты наживки


checkBaitVictory : Maybe Bait -> Int -> List ConductingCard -> ConductingCard -> Bool
checkBaitVictory maybeBait lineTension openTerrainCards fishOutlineCard =
    case maybeBait of
        Nothing ->
            False

        Just bait ->
            case ( fishOutlineCard, List.reverse openTerrainCards ) of
                ( FishOutlineCard { notches }, lastCard :: _ ) ->
                    case lastCard of
                        TerrainCard { notch } ->
                            let
                                ( terrainPos, terrainStr ) =
                                    notch

                                tensionMatch requiredValue =
                                    case bait.tensionMatchCondition of
                                        StrictlyEqual ->
                                            lineTension == requiredValue

                                        AtLeastOrMore x ->
                                            lineTension >= requiredValue + x
                            in
                            case bait.hookPositionCondition of
                                NotchNotConsidered ->
                                    List.any
                                        (\( _, fishStr ) ->
                                            tensionMatch (terrainStr + fishStr)
                                        )
                                        notches

                                NotchConsidered ->
                                    case List.filter (\( pos, _ ) -> pos == terrainPos) notches of
                                        ( _, fishOutlineStr ) :: _ ->
                                            tensionMatch (terrainStr + fishOutlineStr)

                                        [] ->
                                            False

                        FishOutlineCard _ ->
                            False

                _ ->
                    False
