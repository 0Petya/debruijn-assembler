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


compileDot : List String -> Int -> String
compileDot sequences k =
    let
        kmers : List String
        kmers =
            generateKMers sequences k
    in
    "digraph {" ++ String.join "\n" kmers ++ "}"
