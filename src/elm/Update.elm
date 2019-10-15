module Update exposing (update)

import List exposing (..)
import Message exposing (..)
import Model exposing (Model, resetError)
import Ports exposing (renderDot)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckReady ->
            let
                readyToGenerate : Bool
                readyToGenerate =
                    length model.sequences >= 2 && model.k > 0
            in
            ( { model | readyToGenerate = readyToGenerate }, Cmd.none )

        SequenceInput sequenceInput ->
            let
                sequences : List String
                sequences =
                    sequenceInput
                        |> String.lines
                        |> map String.trim
                        |> filter ((<=) 3 << String.length)
            in
            update CheckReady { model | sequences = sequences }

        KInput kInput ->
            case kInput of
                "" ->
                    update CheckReady <| resetError { model | k = 0 }

                otherwise ->
                    case String.toInt kInput of
                        Just k ->
                            update CheckReady <| resetError { model | k = k }

                        Nothing ->
                            ( { model | error = True, errorMessage = "K must be an integer" }, Cmd.none )

        Generate ->
            ( model, renderDot model.dot )
