module Model exposing (Model, initialModel)


type alias Model =
    { sequences : List String
    , dot : String
    }


initialModel : Model
initialModel =
    { sequences = []
    , dot = "digraph  {A -> B}"
    }
