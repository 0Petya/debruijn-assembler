module DeBruijn exposing (Graph, Path, compileDot, compileDotWithPath, findPaths, generateGraph, generateKmers)

import Dict exposing (Dict)
import Set


type alias Node =
    String


type alias Edge =
    ( String, String )


type alias Graph =
    Dict Node (List Node)


type alias Path =
    List Edge


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


compileDot : Graph -> String
compileDot graph =
    let
        digraphConnections : String
        digraphConnections =
            graph
                |> Dict.toList
                |> List.concatMap (\( nodeA, connections ) -> List.map (\nodeB -> ( nodeA, nodeB )) connections)
                |> List.sortBy Tuple.first
                |> List.map (\( nodeA, nodeB ) -> nodeA ++ " -> " ++ nodeB)
                |> String.join "\n"
    in
    "digraph {" ++ digraphConnections ++ "}"


generateDegrees : Graph -> List ( Node, Int )
generateDegrees graph =
    let
        outs : List Node
        outs =
            Dict.keys graph

        ins : List Node
        ins =
            List.concat <| Dict.values graph

        count : comparable -> List comparable -> Int
        count x =
            List.length << List.filter ((==) x)
    in
    List.map (\node -> ( node, (List.length << Maybe.withDefault [] <| Dict.get node graph) - count node ins )) outs


findStartingNode : Graph -> Node
findStartingNode =
    generateDegrees
        >> List.sortBy Tuple.second
        >> List.reverse
        >> List.head
        >> Maybe.map Tuple.first
        >> Maybe.withDefault ""


hasEulerianPath : Graph -> Bool
hasEulerianPath =
    generateDegrees
        >> List.map Tuple.second
        >> List.map (\degree -> ( degree, abs degree ))
        >> List.foldl (\( a, b ) ( aAcc, bBcc ) -> ( a + aAcc, b + bBcc )) ( 0, 0 )
        >> Tuple.mapBoth (\x -> x == 1 || x == 0) (not << (<) 2)
        >> (==) ( True, True )


findPaths : Graph -> List Path
findPaths graph =
    let
        nextPaths : ( List Edge, Node, Graph ) -> List Node -> List ( List Edge, Node, Graph )
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

                        removedConnectingNode : Graph
                        removedConnectingNode =
                            Dict.update node (remove << Maybe.withDefault []) lookup
                    in
                    ( path ++ [ ( node, x ) ], x, removedConnectingNode ) :: nextPaths ( path, node, lookup ) xs

        go : List ( List Edge, Node, Graph ) -> List Path
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
    if not <| hasEulerianPath graph then
        []

    else
        go [ ( [], findStartingNode graph, graph ) ]


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
    nodes
        |> List.map (\node -> ( node, identifyOverlaps node nodes ))
        |> List.filter (not << List.isEmpty << Tuple.second)
        |> Dict.fromList


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
