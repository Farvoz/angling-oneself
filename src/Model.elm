module Model exposing (AppPhase(..), Model, Msg(..), init, update)

import GameModel exposing (GameState)
import Random


type AppPhase
    = MainMenu
    | InGame


type alias Model =
    { appPhase : AppPhase
    , gameState : Maybe GameState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { appPhase = MainMenu
      , gameState = Nothing
      }
    , Cmd.none
    )


type Msg
    = StartNewGame
    | ShuffledDecks Int
    | Restart
    | GameMsg GameModel.GameMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartNewGame ->
            ( { model | appPhase = InGame, gameState = Nothing }
            , Random.generate ShuffledDecks (Random.int Random.minInt Random.maxInt)
            )

        ShuffledDecks seedValue ->
            let
                gameState =
                    GameModel.initialGameState (Random.initialSeed seedValue)
            in
            ( { model | gameState = Just gameState }, Cmd.none )

        Restart ->
            ( { model | gameState = Nothing }
            , Random.generate ShuffledDecks (Random.int Random.minInt Random.maxInt)
            )

        GameMsg gameMsg ->
            case model.gameState of
                Just gameState ->
                    ( { model | gameState = Just (GameModel.updateGame gameMsg gameState) }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )
