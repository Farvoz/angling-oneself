module InitialGameModel exposing (..)

import GameModel exposing (..)
import Random



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
    , FishOutlineCard { notches = [ ( 1, 1 ), ( 2, 1 ) ] }
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
        , distance = 5
        , caughtFish = 0
        , timeElapsed = 0
        , conductingDeck = shuffledConducting
        , techniquesDeck = shuffledTechniques
        , openTerrainCards = []
        , discardedTerrainCards = []
        , openTechniqueCards = []
        , phase = ReadyToCast
        , phaseChanges = []
        , seed = seed2
        , selectedDistance = Nothing
        }
