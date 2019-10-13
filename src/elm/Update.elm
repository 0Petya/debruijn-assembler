module Update exposing (update)

import List exposing (map)
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
                    sequenceInput
                        |> String.lines
                        |> map String.trim
            in
            ( { model | sequences = sequences }, Cmd.none )

        Generate ->
            ( model, renderDot model.dot )
