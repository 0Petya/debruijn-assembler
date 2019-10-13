module Main exposing (main)

import Browser exposing (element)
import Message exposing (Msg)
import Model exposing (Model, initialModel)
import Ports exposing (renderDot)
import Update exposing (update)
import View exposing (view)


main : Program () Model Msg
main =
    element
        { init = always ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
