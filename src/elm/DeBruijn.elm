module DeBruijn exposing (Graph, compileDot, generateEdges, generateKmers, hasEulerianPath, hasOnlyOneComponent)

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


findStartingNode : Graph -> String
findStartingNode =
    generateDegrees
        >> List.sortBy Tuple.second
        >> List.reverse
        >> List.head
        >> Maybe.map Tuple.first
        >> Maybe.withDefault ""


hasOnlyOneComponent : Graph -> Bool
hasOnlyOneComponent graph =
    let
        findConnections : String -> List ( String, String ) -> List String
        findConnections node =
            List.map Tuple.second << List.filter ((==) node << Tuple.first)

        dfs : String -> List ( String, String ) -> List String -> List String
        dfs node edges visited =
            let
                connections : List String
                connections =
                    findConnections node edges

                willHaveVisited : List String
                willHaveVisited =
                    node :: visited
            in
            if List.member node visited then
                []

            else
                willHaveVisited ++ List.concatMap (\x -> dfs x edges willHaveVisited) connections
    in
    dfs (findStartingNode graph) graph.edges []
        |> Set.fromList
        |> Set.diff (Set.fromList graph.nodes)
        |> Set.size
        |> (==) 0


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
