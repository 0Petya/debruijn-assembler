module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Message exposing (..)
import Model exposing (Model)


view : Model -> Html Msg
view model =
    div [ id "elm" ]
        [ h2 [ class "header" ]
            [ text "De Bruijn Graph Generator" ]
        , a [ class "wikipedia-link", href "https://en.wikipedia.org/wiki/De_Bruijn_graph" ]
            [ text "Wikipedia" ]
        , label []
            [ text "Sequences"
            , textarea [ class "sequence-input", onInput SequenceInput ] []
            ]
        , label []
            [ text "k"
            , input [ class "k-input", onInput KInput ] []
            ]
        , button [ class "generate", onClick Generate ]
            [ text "Generate" ]
        , div [ class "error-messages" ] <| List.map (\error -> p [] [ text error ]) model.errors
        ]
