module View exposing (view)

import DeBruijn exposing (hasEulerianPath, hasOnlyOneComponent)
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
    div [ id "elm" ]
        [ h2 [ class "header" ]
            [ text "De Bruijn Graph Generator" ]
        , a [ class "wikipedia-link", href "https://en.wikipedia.org/wiki/De_Bruijn_graph" ]
            [ text "Wikipedia" ]
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
                if hasEulerianPath model.graph && hasOnlyOneComponent model.graph then
                    "Eulerian paths:"

                else
                    "No Eulerian paths"
            ]
        ]
