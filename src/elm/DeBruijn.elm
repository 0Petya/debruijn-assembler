module DeBruijn exposing (compileDot)

import List exposing (..)


generateKMers : List String -> Int -> List String
generateKMers sequences k =
    let
        slidingSlice : String -> List String
        slidingSlice sequence =
            let
                go : String -> List String
                go cutSequence =
                    if String.length cutSequence > k then
                        String.left k cutSequence :: go (String.dropLeft 1 cutSequence)

                    else
                        [ String.left k cutSequence ]
            in
            String.left k sequence :: go (String.dropLeft 1 sequence)
    in
    concatMap slidingSlice sequences


identifyOverlaps : String -> List String -> List String
identifyOverlaps kmer kmers =
    let
        overlapSubject : String
        overlapSubject =
            String.dropLeft 1 kmer
    in
    filter (\target -> String.dropRight 1 target == overlapSubject) kmers


compileDot : List String -> Int -> String
compileDot sequences k =
    let
        kmers : List String
        kmers =
            generateKMers sequences k

        overlapGroups : List ( String, List String )
        overlapGroups =
            map (\kmer -> ( kmer, identifyOverlaps kmer kmers )) kmers

        digraphConnections : ( String, List String ) -> String
        digraphConnections ( kmer, overlaps ) =
            overlaps
                |> map (\overlap -> kmer ++ " -> " ++ overlap)
                |> String.join "\n"
    in
    "digraph {" ++ (String.join "\n" <| map digraphConnections overlapGroups) ++ "}"
