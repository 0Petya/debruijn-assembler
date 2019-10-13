module Update exposing (update)

import Message exposing (..)
import Model exposing (Model)
import Ports exposing (renderDot)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SequenceInput sequenceInput ->
            let
                sequences : List String
                sequences =
                    String.lines sequenceInput
            in
            ( { model | sequences = sequences }, Cmd.none )

        Generate ->
            ( model, renderDot model.dot )
