module Update exposing (update)

import DeBruijn exposing (compileDot)
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
                        |> List.map String.trim
            in
            ( { model | sequences = sequences }, Cmd.none )

        KInput kInput ->
            case String.toInt kInput of
                Just k ->
                    ( { model | k = k }, Cmd.none )

                Nothing ->
                    ( { model | k = 0 }, Cmd.none )

        Generate ->
            ( model, renderDot <| compileDot model.sequences model.k )
