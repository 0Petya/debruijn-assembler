module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Message exposing (..)
import Model exposing (Model)


view : Model -> Html Msg
view model =
    div [ id "elm" ]
        [ textarea [ class "sequence-input", onInput SequenceInput ]
            []
        , button
            [ onClick Generate ]
            [ text "Generate" ]
        ]
