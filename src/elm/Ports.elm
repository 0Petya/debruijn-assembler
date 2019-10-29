port module Ports exposing (..)


port renderDot : String -> Cmd msg


port clearGraph : () -> Cmd msg


port displaySequence : List String -> Cmd msg
