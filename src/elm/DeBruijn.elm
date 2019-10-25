module DeBruijn exposing (Graph, Path, compileDot, compileDotWithPath, findPaths, generateGraph, generateKmers)

import Set


type alias Node =
    String


type alias Edge =
    ( String, String )


type alias Graph =
    List Edge


type alias Path =
    List Edge


findStartingNode : Graph -> Node
findStartingNode =
    generateDegrees
        >> List.sortBy Tuple.second
        >> List.reverse
        >> List.head
        >> Maybe.map Tuple.first
        >> Maybe.withDefault ""


generateDegrees : Graph -> List ( String, Int )
generateDegrees graph =
    let
        outs : List String
        outs =
            List.map Tuple.first graph

        ins : List String
        ins =
            List.map Tuple.second graph

        count : comparable -> List comparable -> Int
        count x =
            List.length << List.filter ((==) x)
    in
    List.map (\node -> ( node, count node outs - count node ins )) outs


findPaths : Graph -> List Path
findPaths graph =
    let
        nextPaths : ( List Edge, Node, List Edge ) -> List Edge -> List ( List Edge, Node, List Edge )
        nextPaths ( path, node, edges ) connectingEdges =
            case connectingEdges of
                [] ->
                    []

                x :: xs ->
                    ( path ++ [ x ], Tuple.second x, List.filter ((/=) x) edges ) :: nextPaths ( path, node, edges ) xs

        go : List ( List Edge, Node, List Edge ) -> List Path
        go acc =
            case acc of
                [] ->
                    []

                ( path, _, [] ) :: xs ->
                    path :: go xs

                ( path, node, edges ) :: xs ->
                    let
                        connectingEdges : List Edge
                        connectingEdges =
                            List.filter ((==) node << Tuple.first) edges
                    in
                    if List.isEmpty connectingEdges then
                        go xs

                    else
                        go (nextPaths ( path, node, edges ) connectingEdges ++ xs)
    in
    go [ ( [], findStartingNode graph, graph ) ]


generateKmers : List String -> Int -> List Node
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


generateGraph : List Node -> Graph
generateGraph nodes =
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


compileDot : Graph -> String
compileDot graph =
    let
        digraphConnections : String
        digraphConnections =
            graph
                |> List.sortBy Tuple.first
                |> List.map (\( nodeA, nodeB ) -> nodeA ++ " -> " ++ nodeB)
                |> String.join "\n"
    in
    "digraph {" ++ digraphConnections ++ "}"


compileDotWithPath : Path -> String
compileDotWithPath path =
    let
        pathConnections : String
        pathConnections =
            path
                |> List.indexedMap (\i ( nodeA, nodeB ) -> ( nodeA, nodeB, i ))
                |> List.sortBy (\( nodeA, _, _ ) -> nodeA)
                |> List.map (\( nodeA, nodeB, i ) -> nodeA ++ " -> " ++ nodeB ++ "[label=\"  " ++ String.fromInt (i + 1) ++ "\"]")
                |> String.join "\n"
    in
    "digraph {" ++ pathConnections ++ "}"
