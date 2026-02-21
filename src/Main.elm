module Main exposing (main)

import Browser
import Html.Styled
import Model exposing (Model, Msg, init, update)
import View exposing (view)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view >> Html.Styled.toUnstyled
        , subscriptions = \_ -> Sub.none
        }
