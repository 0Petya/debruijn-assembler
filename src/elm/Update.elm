module Update exposing (update)

import DeBruijn exposing (Graph, Path, compileDot, compileDotWithPath, findPaths, generateGraph, generateKmers)
import File
import File.Select as Select
import Message exposing (..)
import Model exposing (Model)
import Ports exposing (..)
import Set
import Task


validate : Model -> List String
validate { sequences, k } =
    let
        lengthOfShortestSequence : Int
        lengthOfShortestSequence =
            Maybe.withDefault 0 <| List.minimum (List.map String.length sequences)

        validations : List ( Bool, String )
        validations =
            [ ( List.length sequences < 2, "Please input two or more unique sequences." )
            , ( List.any identity <| List.map (String.any <| not << Char.isAlphaNum) sequences, "Sequences can only contain alphanumeric characters." )
            , ( k <= 1, "Please input a k that is greater than one." )
            , ( lengthOfShortestSequence <= k, "k must be smaller than the smallest sequence." )
            ]
    in
    validations
        |> List.filter (identity << Tuple.first)
        |> List.map Tuple.second


processSequenceFormat : String -> List String
processSequenceFormat sequenceInput =
    let
        split : Int -> List a -> List (List a)
        split i xs =
            case List.take i xs of
                [] ->
                    []

                x ->
                    x :: split i (List.drop i xs)

        processByFormat : List String -> List String
        processByFormat lines =
            if String.contains "@" sequenceInput then
                lines
                    |> split 4
                    |> List.map (Maybe.withDefault "" << List.head << List.drop 1)

            else if String.contains ">" sequenceInput then
                lines
                    |> List.map
                        (\x ->
                            if String.startsWith ">" x then
                                ","

                            else
                                x
                        )
                    |> String.concat
                    |> String.split ","
                    |> List.filter (not << String.isEmpty)

            else
                lines
    in
    sequenceInput
        |> String.lines
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)
        |> processByFormat
        |> (Set.toList << Set.fromList)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SequenceInput sequenceInput ->
            ( { model | sequences = processSequenceFormat sequenceInput }, Cmd.none )

        SequenceUpload ->
            ( model, Select.file [ "text/plain" ] SequenceSelected )

        SequenceSelected file ->
            ( { model | sequenceUploadFileName = Just <| File.name file }, Task.perform SequenceLoaded (File.toString file) )

        SequenceLoaded contents ->
            update (SequenceInput contents) model

        KInput kInput ->
            case String.toInt << String.trim <| kInput of
                Just k ->
                    ( { model | k = k }, Cmd.none )

                Nothing ->
                    ( { model | k = 0 }, Cmd.none )

        Generate ->
            case validate model of
                [] ->
                    let
                        graph : Graph
                        graph =
                            generateGraph <| generateKmers model.sequences model.k

                        paths : List Path
                        paths =
                            findPaths graph
                    in
                    ( { model | currentPath = [], paths = paths, isGenerated = True, errors = [] }, Cmd.batch [ renderDot <| compileDot graph, displaySequence "&#8203;" ] )

                errors ->
                    ( { model | currentPath = [], isGenerated = False, errors = errors }, Cmd.batch [ clearGraph (), displaySequence "&#8203;" ] )

        ViewPath path ->
            let
                sequence : String
                sequence =
                    path
                        |> List.map (String.right 1 << Tuple.second)
                        |> String.join ""
                        |> (++) (Maybe.withDefault "" << Maybe.map Tuple.first <| List.head path)
            in
            ( { model | currentPath = path }, Cmd.batch [ displaySequence sequence, renderDot <| compileDotWithPath path ] )
