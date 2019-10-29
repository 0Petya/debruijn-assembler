module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Message exposing (..)
import Model exposing (Model)


isNotNothing : Maybe a -> Bool
isNotNothing m =
    case m of
        Nothing ->
            False

        Just _ ->
            True


view : Model -> Html Msg
view model =
    div [ id "panel" ]
        [ h2 [ class "header" ]
            [ text "De Bruijn Graph Sequence Assembler" ]
        , a [ href "https://en.wikipedia.org/wiki/De_Bruijn_graph" ]
            [ text "Wikipedia" ]
        , a [ class "source-link", href "https://github.com/0Petya/debruijn-assembler" ]
            [ text "Source" ]
        , label []
            [ text "FASTQ, FASTA, or raw"
            , textarea [ class "sequence-input", disabled <| isNotNothing model.sequenceUploadFileName, onInput SequenceInput ] []
            ]
        , div [ class "file-upload" ]
            [ button [ onClick SequenceUpload ] [ text "Select File" ]
            , text <| Maybe.withDefault "No file selected" model.sequenceUploadFileName
            ]
        , label []
            [ text "k"
            , input [ class "k-input", onInput KInput ] []
            ]
        , button [ class "generate", onClick Generate ]
            [ text "Generate" ]
        , div [ class "error-messages" ] <| List.map (\error -> p [] [ text error ]) model.errors
        , p [ class "has-eulerian-path", hidden <| not model.isGenerated ]
            [ text <|
                if List.isEmpty model.paths then
                    "No Eulerian paths"

                else
                    "Eulerian paths:"
            ]
        , div [ class "paths" ]
            (List.indexedMap
                (\i path ->
                    button
                        [ classList
                            [ ( "path", True )
                            , ( "current-path", path == model.currentPath )
                            ]
                        , onClick (ViewPath path)
                        ]
                        [ text << String.fromInt <| i + 1 ]
                )
                model.paths
            )
        , button
            [ classList
                [ ( "cut-repeats", True )
                , ( "current-path", model.currentPath == [ ( "repeat", "repeat" ) ] )
                ]
            , onClick CutRepeats
            , hidden <| List.length model.paths <= 1
            ]
            [ text "Cut Out Repeats" ]
        ]
