module Update exposing (update)

import DeBruijn exposing (compileDot)
import Message exposing (..)
import Model exposing (Model)
import Ports exposing (clearGraph, renderDot)
import Set


validate : Model -> List String
validate { sequences, k } =
    let
        lengthOfShortestSequence : Int
        lengthOfShortestSequence =
            Maybe.withDefault 0 <| List.minimum (List.map String.length sequences)

        validations : List ( Bool, String )
        validations =
            [ ( List.length sequences < 2, "Please input two or more sequences." )
            , ( k <= 2, "Please input a k greater than two." )
            , ( lengthOfShortestSequence <= k, "k must be smaller than the smallest sequence." )
            ]
    in
    validations
        |> List.filter (identity << Tuple.first)
        |> List.map Tuple.second


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
                        |> List.filter (not << String.isEmpty)
                        |> (Set.toList << Set.fromList)
            in
            ( { model | sequences = sequences }, Cmd.none )

        KInput kInput ->
            case String.toInt kInput of
                Just k ->
                    ( { model | k = k }, Cmd.none )

                Nothing ->
                    ( { model | k = 0 }, Cmd.none )

        Generate ->
            case validate model of
                [] ->
                    ( { model | errors = [] }, renderDot <| compileDot model.sequences model.k )

                errors ->
                    ( { model | errors = errors }, clearGraph () )
