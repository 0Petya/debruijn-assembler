module Main exposing (main)

import Browser exposing (element)
import Message exposing (Msg)
import Model exposing (Model)
import Ports exposing (renderDot)
import Update exposing (update)
import View exposing (view)


test_dot : String
test_dot =
    "digraph  {A -> B}"


main : Program () Model Msg
main =
    element
        { init = always ( { dot = test_dot }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
