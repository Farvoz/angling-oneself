module GameModel.Phases exposing (..)

-- Фазы игры


type GamePhase
    = ReadyToCast -- Готов к забросу
    | Conducting -- Проводка
    | Fighting -- Вываживание
    | TechniqueChoice GamePhase -- Выбор приёма, вернуться в указанную фазу


type alias PhaseChange =
    { from : GamePhase
    , to : GamePhase
    , reason : String
    }


maxPhaseChanges : Int
maxPhaseChanges =
    8


transitionPhase : ( GamePhase, GamePhase ) -> String -> List PhaseChange -> { phase : GamePhase, phaseChanges : List PhaseChange }
transitionPhase ( fromPhase, toPhase ) reason prevPhaseChanges =
    let
        entry =
            { from = fromPhase
            , to = toPhase
            , reason = reason
            }
    in
    { phase = toPhase
    , phaseChanges = List.take maxPhaseChanges (entry :: prevPhaseChanges)
    }
