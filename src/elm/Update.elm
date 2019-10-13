module Update exposing (update)

import Message exposing (..)
import Model exposing (Model)
import Ports exposing (renderDot)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( model, renderDot model.dot )
