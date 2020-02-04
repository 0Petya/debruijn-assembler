port module Ports exposing (clearGraph, displaySequence, renderDot)


port renderDot : String -> Cmd msg


port clearGraph : () -> Cmd msg


port displaySequence : List String -> Cmd msg
