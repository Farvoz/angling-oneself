module GameModel exposing
    ( Bait
    , ConductingCard(..)
    , GameMsg(..)
    , GamePhase(..)
    , GameState
    , HookPositionCondition(..)
    , Notch
    , PhaseChange
    , TechniqueCard(..)
    , TensionChangeCondition(..)
    , TensionMatchCondition(..)
    , TensionMode(..)
    , checkBaitVictory
    , drawCard
    , shuffleAndPrepend
    , shuffleList
    , updateGame
    )

import Array
import Random


-- Типы для колоды проводки

-- (Позиция, Сила)
type alias Notch
    = (Int, Int)

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
    = NotchNotConsidered   -- не учитывается положение засечки
    | NotchConsidered      -- учитывается положение засечки

type TensionMatchCondition
    = StrictlyEqual        -- строго равно
    | AtLeastOrMore Int    -- такое или на Х больше (X — параметр)

type TensionChangeCondition
    = BreaksOnSuddenPlus2  -- рвётся при резком +2
    | NoEffect             -- не влияет

type alias Bait =
    { maxTension : Int
    , hookPositionCondition : HookPositionCondition
    , tensionMatchCondition : TensionMatchCondition
    , tensionChangeCondition : TensionChangeCondition
    }


-- Фазы игры

type GamePhase
    = ReadyToCast   -- Готов к забросу
    | Conducting    -- Проводка
    | Fighting      -- Вываживание


type alias PhaseChange =
    { from : GamePhase
    , to : GamePhase
    , reason : String
    }


maxPhaseChanges : Int
maxPhaseChanges =
    8


transitionPhase : GamePhase -> String -> GameState -> GameState
transitionPhase toPhase reason gameState =
    if gameState.phase == toPhase then
        gameState

    else
        let
            entry =
                { from = gameState.phase
                , to = toPhase
                , reason = reason
                }
        in
        { gameState
            | phase = toPhase
            , phaseChanges = List.take maxPhaseChanges (entry :: gameState.phaseChanges)
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
    gameState
        |> transitionPhase ReadyToCast reason


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


moveTopToBottom : Int -> List a -> List a
moveTopToBottom n deck =
    let
        top =
            List.take n deck
        rest =
            List.drop n deck
    in
    rest ++ top


removeAt : Int -> List a -> List a
removeAt index list =
    List.take index list ++ List.drop (index + 1) list


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
                                        newTension > bait.maxTension
                                            || (bait.tensionChangeCondition == BreaksOnSuddenPlus2
                                                    && (newTension - gameState.lineTension) >= 2
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
                                                baseState
                                                    |> (\s -> { s | distance = newDistance })
                                                    |> transitionPhase Fighting "клюнуло на наживку"
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

        Cast ->
            case gameState.selectedDistance of
                Just n ->
                    if gameState.phase == ReadyToCast
                        && n >= 2
                        && n <= 10
                        && gameState.equippedBaitIndex /= Nothing
                        && not (List.isEmpty gameState.availableBaits)
                    then
                        gameState
                            |> resetForCast
                            |> (\s -> { s | distance = n, selectedDistance = Nothing })
                            |> transitionPhase Conducting ("выбрана дистанция " ++ String.fromInt n)
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

-- Функция перемешивания списка (Fisher-Yates shuffle)

shuffleList : Random.Seed -> List a -> ( List a, Random.Seed )
shuffleList seed list =
    let
        length =
            List.length list

        array =
            Array.fromList list
    in
    shuffleArray seed array 0 length


shuffleArray : Random.Seed -> Array.Array a -> Int -> Int -> ( List a, Random.Seed )
shuffleArray seed array currentIndex length =
    if currentIndex >= length then
        ( Array.toList array, seed )
    else
        let
            ( randomIndex, newSeed ) =
                Random.step (Random.int currentIndex (length - 1)) seed

            swappedArray =
                swap array currentIndex randomIndex
        in
        shuffleArray newSeed swappedArray (currentIndex + 1) length


swap : Array.Array a -> Int -> Int -> Array.Array a
swap array i j =
    case ( Array.get i array, Array.get j array ) of
        ( Just a, Just b ) ->
            array
                |> Array.set i b
                |> Array.set j a

        _ ->
            array


-- Достать верхнюю карту из колоды

drawCard : List ConductingCard -> ( List ConductingCard, Maybe ConductingCard )
drawCard deck =
    case deck of
        [] ->
            ( [], Nothing )

        first :: rest ->
            ( rest, Just first )


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
                                (terrainPos, terrainStr) = notch
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


-- Перемешать список и добавить в начало колоды

shuffleAndPrepend : Random.Seed -> List ConductingCard -> List ConductingCard -> ( List ConductingCard, Random.Seed )
shuffleAndPrepend seed cardsToShuffle deck =
    let
        ( shuffled, newSeed ) =
            shuffleList seed cardsToShuffle
    in
    ( shuffled ++ deck, newSeed )
