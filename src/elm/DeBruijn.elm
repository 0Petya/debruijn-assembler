module DeBruijn exposing (compileDot)

import Set


unique : List comparable -> List comparable
unique =
    Set.toList << Set.fromList


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
    unique <| List.concatMap slidingSlice sequences


identifyOverlaps : String -> List String -> List String
identifyOverlaps kmer =
    let
        overlapSubject : String
        overlapSubject =
            String.dropLeft 1 kmer
    in
    unique << List.filter (\target -> String.dropRight 1 target == overlapSubject)


compileDot : List String -> Int -> String
compileDot sequences k =
    let
        kmers : List String
        kmers =
            generateKMers sequences k

        overlapGroups : List ( String, List String )
        overlapGroups =
            List.map (\kmer -> ( kmer, identifyOverlaps kmer kmers )) kmers

        digraphConnections : ( String, List String ) -> String
        digraphConnections ( kmer, overlaps ) =
            overlaps
                |> List.map (\overlap -> kmer ++ " -> " ++ overlap)
                |> String.join "\n"
    in
    "digraph {" ++ (String.join "\n" <| List.map digraphConnections overlapGroups) ++ "}"
