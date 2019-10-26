module DeBruijn exposing (EdgeLookup, Graph, Path, compileDot, compileDotWithPath, findPaths, generateGraph, generateKmers)

import Dict exposing (Dict)
import Set


type alias Node =
    String


type alias Edge =
    ( String, String )


type alias Graph =
    List Edge


type alias EdgeLookup =
    Dict Node (List Node)


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


findPaths : Graph -> EdgeLookup -> List Path
findPaths graph edgeLookup =
    let
        nextPaths : ( List Edge, Node, EdgeLookup ) -> List Node -> List ( List Edge, Node, EdgeLookup )
        nextPaths ( path, node, lookup ) connectingNodes =
            case connectingNodes of
                [] ->
                    []

                x :: xs ->
                    let
                        remove : List Node -> Maybe (List Node)
                        remove nodes =
                            let
                                removedNode : List Node
                                removedNode =
                                    List.filter ((/=) x) nodes
                            in
                            if List.isEmpty removedNode then
                                Nothing

                            else
                                Just removedNode

                        removedConnectingNode : EdgeLookup
                        removedConnectingNode =
                            Dict.update node (remove << Maybe.withDefault []) lookup
                    in
                    ( path ++ [ ( node, x ) ], x, removedConnectingNode ) :: nextPaths ( path, node, lookup ) xs

        go : List ( List Edge, Node, EdgeLookup ) -> List Path
        go acc =
            case acc of
                [] ->
                    []

                ( path, node, lookup ) :: xs ->
                    if Dict.isEmpty lookup then
                        path :: go xs

                    else
                        let
                            connectingNodes : List Node
                            connectingNodes =
                                Maybe.withDefault [] <| Dict.get node lookup
                        in
                        if List.isEmpty connectingNodes then
                            go xs

                        else
                            go (nextPaths ( path, node, lookup ) connectingNodes ++ xs)
    in
    go [ ( [], findStartingNode graph, edgeLookup ) ]


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


generateGraph : List Node -> ( Graph, EdgeLookup )
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

        overlapLookup : List ( String, List String )
        overlapLookup =
            List.filter (not << List.isEmpty << Tuple.second) <| List.map (\node -> ( node, identifyOverlaps node nodes )) nodes
    in
    ( List.concatMap (\( node, overlaps ) -> List.map (\overlap -> ( node, overlap )) overlaps) overlapLookup, Dict.fromList overlapLookup )


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
