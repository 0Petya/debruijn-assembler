module Dot exposing (compileDot, compileDotWithPath)

import DeBruijn exposing (Graph, Node, Path)
import Dict


compileDotWithPath : Path -> String
compileDotWithPath path =
    let
        pathConnections : String
        pathConnections =
            case path of
                [] ->
                    ""

                [ ( node, _ ) ] ->
                    node

                _ ->
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
        formConnection : ( Node, Node ) -> String
        formConnection ( nodeA, nodeB ) =
            if String.isEmpty nodeB then
                nodeA

            else
                nodeA ++ " -> " ++ nodeB

        digraphConnections : String
        digraphConnections =
            graph
                |> Dict.toList
                |> List.concatMap
                    (\( nodeA, connections ) ->
                        if List.isEmpty connections then
                            [ ( nodeA, "" ) ]

                        else
                            List.map (\nodeB -> ( nodeA, nodeB )) connections
                    )
                |> List.sortBy Tuple.first
                |> List.map formConnection
                |> String.join "\n"
    in
    "digraph {" ++ digraphConnections ++ "}"
