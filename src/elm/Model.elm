module Model exposing (Model, initialModel)


type alias Model =
    { sequences : List String
    , readyToGenerate : Bool
    , dot : String
    }


initialModel : Model
initialModel =
    { sequences = []
    , readyToGenerate = False
    , dot = "digraph  {A -> B}"
    }
