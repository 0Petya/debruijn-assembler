module DeBruijn exposing (Graph, compileDot, generateEdges, generateKmers, hasEulerianPath)

import Set


type alias Graph =
    { nodes : List String
    , edges : List ( String, String )
    }


generateDegrees : Graph -> List ( String, Int )
generateDegrees { nodes, edges } =
    let
        outs : List String
        outs =
            List.map Tuple.first edges

        ins : List String
        ins =
            List.map Tuple.second edges

        count : comparable -> List comparable -> Int
        count x =
            List.length << List.filter ((==) x)
    in
    List.map (\node -> ( node, count node outs - count node ins )) nodes


hasEulerianPath : Graph -> Bool
hasEulerianPath =
    generateDegrees
        >> List.map Tuple.second
        >> List.map (\degree -> ( degree, abs degree ))
        >> List.foldl (\( degree, mag ) ( degreeAcc, magAcc ) -> ( degree + degreeAcc, mag + magAcc )) ( 0, 0 )
        >> Tuple.mapBoth ((==) 0) (not << (<) 2)
        >> (==) ( True, True )


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
