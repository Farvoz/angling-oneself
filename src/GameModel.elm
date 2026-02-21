module GameModel exposing (ConductingCard(..), GameMsg(..), GamePhase(..), GameState, NotchPosition(..), TechniqueCard, checkBaitVictory, drawCard, initialConductingDeck, initialGameState, initialTechniquesDeck, shuffleAndPrepend, shuffleList, updateGame)

import Array
import Random


-- Типы для колоды проводки

type NotchPosition
    = TopLeft
    | TopRight
    | MiddleLeft
    | MiddleRight
    | BottomLeft
    | BottomRight


type ConductingCard
    = TerrainCard { tension : Int, notchPosition : NotchPosition, notchStrength : Int }
    | BaitCard { notches : List ( NotchPosition, Int ) }


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
                                        gameState.lineTension + tension
                                in
                                { gameState
                                    | conductingDeck = newDeck
                                    , openTerrainCards = newOpenCards
                                    , lineTension = newTension
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
            }

        SearchNewPlace ->
            { gameState
                | discardedTerrainCards = gameState.discardedTerrainCards ++ gameState.openTerrainCards
                , openTerrainCards = []
            }

        ShuffledStayHere shuffledCards ->
            { gameState
                | conductingDeck = shuffledCards ++ gameState.conductingDeck
                , openTerrainCards = []
            }


-- Начальная колода проводки

initialConductingDeck : List ConductingCard
initialConductingDeck =
    [ TerrainCard { tension = 1, notchPosition = TopRight, notchStrength = 1 }
    , TerrainCard { tension = 1, notchPosition = MiddleRight, notchStrength = 1 }
    , TerrainCard { tension = 1, notchPosition = BottomRight, notchStrength = 1 }
    , TerrainCard { tension = -1, notchPosition = TopRight, notchStrength = 1 }
    , TerrainCard { tension = -1, notchPosition = MiddleRight, notchStrength = 1 }
    , TerrainCard { tension = -1, notchPosition = BottomRight, notchStrength = 1 }
    , TerrainCard { tension = 1, notchPosition = TopRight, notchStrength = 1 }
    , TerrainCard { tension = 1, notchPosition = MiddleRight, notchStrength = 1 }
    , TerrainCard { tension = 2, notchPosition = TopRight, notchStrength = 1 }
    , BaitCard { notches = [ ( TopLeft, 1 ), ( MiddleLeft, 1 ) ] }
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
                TerrainCard { notchPosition, notchStrength } ->
                    -- Проверить совпадение уровня натяжения и засечки карты местности
                    -- Также проверим, что в карте наживки есть засечка в этой позиции
                    let
                        hasMatchingPosition =
                            List.any (\( pos, _ ) -> pos == notchPosition) notches
                    in
                    hasMatchingPosition && lineTension == notchStrength

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
