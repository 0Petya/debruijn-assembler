module Main exposing (main)

import Browser
import Message exposing (Msg)
import Model exposing (Model, initialModel)
import Update exposing (update)
import View exposing (view)


main : Program () Model Msg
main =
    Browser.element
        { init = always ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
