module Update exposing (update)

import List exposing (..)
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
                        |> filter ((<=) 3 << String.length)

                readyToGenerate : Bool
                readyToGenerate =
                    length sequences >= 2
            in
            ( { model | sequences = sequences, readyToGenerate = readyToGenerate }, Cmd.none )

        Generate ->
            ( model, renderDot model.dot )
