module Model exposing (Model, initialModel)


type alias Model =
    { sequences : List String
    , k : Int
    }


initialModel : Model
initialModel =
    { sequences = []
    , k = 0
    }
