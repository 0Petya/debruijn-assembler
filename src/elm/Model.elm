module Model exposing (Model, initialModel)


type alias Model =
    { sequences : List String
    , k : Int
    , dot : String
    }


initialModel : Model
initialModel =
    { sequences = []
    , k = 0
    , dot = "digraph  {A -> B}"
    }
