module DeBruijn exposing (Graph, Path, compileDot, compileDotWithPath, compressGraph, findPaths, formSequenceFromPath, generateDegrees, generateGraph, generateKmers, getPathsFromResolvedGraph, resolveRepeats)

import Dict exposing (Dict)
import Set


type alias Node =
    String


type alias Edge =
    ( Node, Node )


type alias Graph =
    Dict Node (List Node)


type alias Path =
    List Edge


getPathsFromResolvedGraph : Graph -> List Node -> List Path
getPathsFromResolvedGraph cutGraph repeats =
    let
        getAStart : Graph -> Maybe Node
        getAStart =
            generateDegrees
                >> List.filter ((<) 0 << Tuple.second)
                >> List.head
                >> Maybe.map Tuple.first

        getPaths : ( Path, Maybe Node, Graph ) -> List Path
        getPaths ( path, node, edges ) =
            case node of
                Nothing ->
                    []

                Just nodeA ->
                    let
                        connections : Maybe (List Node)
                        connections =
                            Dict.get nodeA edges

                        culledEdges : Graph
                        culledEdges =
                            Dict.remove nodeA edges
                    in
                    case connections of
                        Nothing ->
                            List.reverse path :: getPaths ( [], getAStart culledEdges, culledEdges )

                        Just xs ->
                            case xs of
                                [] ->
                                    List.reverse path :: getPaths ( [], getAStart culledEdges, culledEdges )

                                nodeB :: _ ->
                                    getPaths ( ( nodeA, nodeB ) :: path, Just nodeB, culledEdges )
    in
    cutGraph
        |> Dict.filter (\node _ -> not <| List.member node repeats)
        |> (\edges -> getPaths ( [], getAStart edges, edges ))


resolveRepeats : Graph -> ( Graph, List Node )
resolveRepeats graph =
    let
        repeats : List Node
        repeats =
            Dict.keys <| Dict.filter (\_ connections -> List.length connections > 1) graph
    in
    ( Dict.map
        (\node connections ->
            if List.member node repeats then
                [ "" ]

            else
                List.filter (\connection -> not <| List.member connection repeats) connections
        )
        graph
    , repeats
    )


formSequenceFromPath : Path -> String
formSequenceFromPath path =
    path
        |> List.map (String.right 1 << Tuple.second)
        |> String.join ""
        |> (++) (Maybe.withDefault "" << Maybe.map Tuple.first <| List.head path)


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
                |> List.concatMap (\( nodeA, connections ) -> List.map (\nodeB -> ( nodeA, nodeB )) connections)
                |> List.sortBy Tuple.first
                |> List.map formConnection
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


compressGraph : Graph -> Graph
compressGraph graph =
    let
        compressible : List Node
        compressible =
            List.map Tuple.first << List.filter ((>=) 1 << abs << Tuple.second) <| generateDegrees graph

        findPrevious : Node -> Maybe Node
        findPrevious node =
            List.head << Dict.keys <| Dict.filter (\previous connections -> List.member node connections) graph

        compressibleRoots : List Node
        compressibleRoots =
            let
                isCompressibleRoot : Node -> Bool
                isCompressibleRoot node =
                    case findPrevious node of
                        Nothing ->
                            True

                        Just parent ->
                            not <| List.member parent compressible
            in
            List.filter isCompressibleRoot compressible

        findNextCompressible : Node -> Graph -> Maybe Node
        findNextCompressible node =
            Maybe.andThen (List.head << List.filter (\next -> List.member next compressible)) << Dict.get node

        compress : List Node -> Graph -> Graph
        compress toCompress graphAcc =
            case toCompress of
                [] ->
                    graphAcc

                root :: roots ->
                    let
                        updatedWithCompressedSection : Graph
                        updatedWithCompressedSection =
                            let
                                compressSection : Node -> List Node -> Path
                                compressSection node visited =
                                    case findNextCompressible node graphAcc of
                                        Nothing ->
                                            []

                                        Just next ->
                                            if List.member next visited then
                                                []

                                            else
                                                ( node, next ) :: compressSection next (node :: visited)

                                compressedSection : Path
                                compressedSection =
                                    compressSection root []

                                compressedNodes : List Node
                                compressedNodes =
                                    List.concat <| List.map (\( a, b ) -> [ a, b ]) compressedSection

                                lastCompressedNode : Node
                                lastCompressedNode =
                                    Tuple.second << Maybe.withDefault ( "", "" ) << List.head <| List.reverse compressedSection

                                sequence : Node
                                sequence =
                                    formSequenceFromPath compressedSection

                                lastConnections : List Node
                                lastConnections =
                                    case Dict.get lastCompressedNode graphAcc of
                                        Nothing ->
                                            []

                                        Just connections ->
                                            if List.member lastCompressedNode connections then
                                                []

                                            else
                                                connections

                                updatePrevious : Graph -> Graph
                                updatePrevious graphAcc_ =
                                    case findPrevious root of
                                        Nothing ->
                                            graphAcc_

                                        Just previous ->
                                            Dict.update previous
                                                (Just
                                                    << List.map
                                                        (\connection ->
                                                            if connection == root then
                                                                sequence

                                                            else
                                                                connection
                                                        )
                                                    << Maybe.withDefault []
                                                )
                                                graphAcc_
                            in
                            graphAcc
                                |> Dict.filter (\node _ -> not <| List.member node compressedNodes)
                                |> Dict.insert sequence lastConnections
                                |> updatePrevious
                    in
                    compress roots updatedWithCompressedSection
    in
    compress compressibleRoots graph


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
