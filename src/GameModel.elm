module GameModel exposing (
    ConductingCard(..), 
    GameMsg(..), GamePhase(..), GameState, Notch, TechniqueCard, checkBaitVictory, drawCard, initialConductingDeck, initialGameState, initialTechniquesDeck, shuffleAndPrepend, shuffleList, updateGame
    , TensionMode(..))

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
    | BaitCard { notches : List Notch }

maxTension : number
maxTension = 3

-- Типы для колоды приёмов

type TechniqueCard
    = Strike Int
    | Maneuver Int
    | LoosenDrag Int


-- Фазы игры

type GamePhase
    = Playing
    | Won
    | Lost



-- Игровое состояние

type alias GameState =
    { lineTension : Int
    , caughtFish : Int
    , timeElapsed : Int
    , conductingDeck : List ConductingCard
    , techniquesDeck : List TechniqueCard
    , openTerrainCards : List ConductingCard
    , discardedTerrainCards : List ConductingCard
    , openTechniqueCards : List TechniqueCard
    , phase : GamePhase
    , seed : Random.Seed
    }


-- Игровые сообщения

type GameMsg
    = Pull
    | StayHere
    | SearchNewPlace
    | ShuffledStayHere (List ConductingCard)


updateGame : GameMsg -> GameState -> GameState
updateGame msg gameState =
    case msg of
        Pull ->
            if gameState.phase /= Playing then
                gameState
            else
                let
                    ( newDeck, maybeCard ) =
                        drawCard gameState.conductingDeck
                in
                case maybeCard of
                    Nothing ->
                        { gameState | phase = Lost }

                    Just card ->
                        case card of
                            TerrainCard { tension } ->
                                let
                                    newOpenCards =
                                        gameState.openTerrainCards ++ [ card ]

                                    newTension =
                                        case tension.mode of
                                            TensionSet ->
                                                tension.value
                                            TensionChange ->
                                                gameState.lineTension + tension.value

                                    isLost = 
                                        newTension > maxTension
                                in
                                { gameState
                                    | conductingDeck = newDeck
                                    , openTerrainCards = newOpenCards
                                    , lineTension = newTension
                                    , phase = 
                                        if isLost then
                                            Lost
                                        else
                                            Playing
                                }

                            BaitCard _ ->
                                let
                                    isVictory =
                                        checkBaitVictory gameState.lineTension gameState.openTerrainCards card

                                    newPhase =
                                        if isVictory then
                                            Won
                                        else
                                            Playing
                                in
                                { gameState
                                    | conductingDeck = newDeck
                                    , openTerrainCards = gameState.openTerrainCards ++ [ card ]
                                    , phase = newPhase
                                }

        StayHere ->
            let
                ( shuffledCards, seed1 ) =
                    shuffleAndPrepend gameState.seed gameState.openTerrainCards gameState.conductingDeck
            in
            { gameState
                | conductingDeck = shuffledCards
                , openTerrainCards = []
                , seed = seed1
                , timeElapsed = gameState.timeElapsed + 1
            }

        SearchNewPlace ->
            { gameState
                | discardedTerrainCards = gameState.discardedTerrainCards ++ gameState.openTerrainCards
                , openTerrainCards = []
                , timeElapsed = gameState.timeElapsed + List.length gameState.openTerrainCards
            }

        ShuffledStayHere shuffledCards ->
            { gameState
                | conductingDeck = shuffledCards ++ gameState.conductingDeck
                , openTerrainCards = []
            }


-- Начальная колода проводки

initialConductingDeck : List ConductingCard
initialConductingDeck =
    [ TerrainCard { tension = { mode = TensionSet, value = 1 }, notch = (1, 1) }
    , TerrainCard { tension = { mode = TensionSet, value = 1 }, notch = (2, 1) }
    , TerrainCard { tension = { mode = TensionSet, value = 1 }, notch = (3, 1) }
    , TerrainCard { tension = { mode = TensionChange, value = -1 }, notch = (1, 1) }
    , TerrainCard { tension = { mode = TensionChange, value = -1 }, notch = (2, 1) }
    , TerrainCard { tension = { mode = TensionChange, value = -1 }, notch = (3, 1) }
    , TerrainCard { tension = { mode = TensionChange, value = 1 }, notch = (1, 1) }
    , TerrainCard { tension = { mode = TensionChange, value = 1 }, notch = (3, 1)}
    , TerrainCard { tension = { mode = TensionChange, value = 2 }, notch = (1, 1)}
    , BaitCard { notches = [ ( 1, 1 ), ( 2, 1 ) ] }
    ]


-- Начальная колода приёмов

initialTechniquesDeck : List TechniqueCard
initialTechniquesDeck =
    [ Strike 1
    , Strike 1
    , Strike 1
    , Maneuver -1
    , Maneuver -1
    , Maneuver -1
    , LoosenDrag 1
    , LoosenDrag 1
    ]


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

checkBaitVictory : Int -> List ConductingCard -> ConductingCard -> Bool
checkBaitVictory lineTension openTerrainCards baitCard =
    case ( baitCard, List.reverse openTerrainCards ) of
        ( BaitCard { notches }, lastCard :: _ ) ->
            case lastCard of
                TerrainCard { notch } ->
                    -- Проверить совпадение уровня натяжения и засечки карты местности
                    -- Также проверим, что в карте наживки есть засечка в этой позиции
                    let
                        (terrainPos, terrainStr) = notch
                        matchedNotches =
                            List.filter (\( pos, _ ) -> pos == terrainPos) notches
                    in
                        case List.head matchedNotches of
                            Just (baitPos, baitStr) ->
                                baitPos == terrainPos && lineTension == (terrainStr + baitStr)
                            Nothing ->
                                False

                BaitCard _ ->
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


-- Начальное игровое состояние с перемешанными колодами

initialGameState : Random.Seed -> GameState
initialGameState seed =
    let
        ( shuffledConducting, seed1 ) =
            shuffleList seed initialConductingDeck

        ( shuffledTechniques, seed2 ) =
            shuffleList seed1 initialTechniquesDeck
    in
        { lineTension = 0
        , caughtFish = 0
        , timeElapsed = 0
        , conductingDeck = shuffledConducting
        , techniquesDeck = shuffledTechniques
        , openTerrainCards = []
        , discardedTerrainCards = []
        , openTechniqueCards = []
        , phase = Playing
        , seed = seed2
        }
