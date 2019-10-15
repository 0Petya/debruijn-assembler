module Model exposing (Model, initialModel)


type alias Model =
    { sequences : List String
    , k : Int
    , kmers : List String
    }


initialModel : Model
initialModel =
    { sequences = []
    , k = 0
    , kmers = []
    }
