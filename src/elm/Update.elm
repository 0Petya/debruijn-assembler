module Update exposing (update)

import DeBruijn exposing (Graph, compressGraph, findPaths, formSequenceFromPath, generateGraph, generateKmers, getPathsFromResolvedGraph, resolveRepeats)
import Dot exposing (compileDot, compileDotWithPath)
import File
import File.Select as Select
import Message exposing (Msg(..))
import Model exposing (Model)
import Ports exposing (clearGraph, displaySequence, renderDot)
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
            [ ( List.any identity <| List.map (String.any <| not << Char.isAlphaNum) sequences, "Sequences can only contain alphanumeric characters." )
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
                    in
                    ( { model | currentPath = [], graph = graph, paths = findPaths graph, isGenerated = True, errors = [] }, Cmd.batch [ renderDot <| compileDot graph, displaySequence [ "&#8203;" ] ] )

                errors ->
                    ( { model | currentPath = [], isGenerated = False, errors = errors }, Cmd.batch [ clearGraph (), displaySequence [ "&#8203;" ] ] )

        Compress ->
            let
                compressed : Graph
                compressed =
                    compressGraph model.graph
            in
            ( { model | currentPath = [], graph = compressed, paths = findPaths compressed }, Cmd.batch [ renderDot <| compileDot compressed, displaySequence [ "&#8203;" ] ] )

        ViewPath path ->
            ( { model | currentPath = path }, Cmd.batch [ displaySequence [ formSequenceFromPath path ], renderDot <| compileDotWithPath path ] )

        CutRepeats ->
            let
                ( cutGraph, repeats ) =
                    resolveRepeats model.graph

                uniqueSequences : String
                uniqueSequences =
                    (++) "Unique: " << String.join " " << List.map formSequenceFromPath <| getPathsFromResolvedGraph cutGraph repeats

                repeatSequences : String
                repeatSequences =
                    (++) "Repeat: " <| String.join " " repeats
            in
            ( { model | currentPath = [ ( "repeat", "repeat" ) ] }, Cmd.batch [ displaySequence [ uniqueSequences, repeatSequences ], renderDot <| compileDot cutGraph ] )
