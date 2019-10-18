module DeBruijn exposing (Graph, compileDot, generateEdges, generateKmers)

import Set


type alias Graph =
    { nodes : List String
    , edges : List ( String, String )
    }


generateKmers : List String -> Int -> List String
generateKmers sequences k =
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
    Set.toList << Set.fromList <| List.concatMap slidingSlice sequences


generateEdges : List String -> List ( String, String )
generateEdges nodes =
    let
        identifyOverlaps : String -> List String -> List String
        identifyOverlaps kmer =
            let
                overlapSubject : String
                overlapSubject =
                    String.dropLeft 1 kmer
            in
            Set.toList << Set.fromList << List.filter (\target -> String.dropRight 1 target == overlapSubject)
    in
    List.concatMap (\node -> List.map (\overlap -> ( node, overlap )) <| identifyOverlaps node nodes) nodes


compileDot : List ( String, String ) -> String
compileDot edges =
    let
        digraphConnections : String
        digraphConnections =
            String.join "\n" <| List.map (\( nodeA, nodeB ) -> nodeA ++ " -> " ++ nodeB) edges
    in
    "digraph {" ++ digraphConnections ++ "}"
