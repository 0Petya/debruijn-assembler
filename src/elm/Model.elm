module Model exposing (Model, initialModel)


type alias Model =
    { sequences : List String
    , sequenceUploadFileName : Maybe String
    , k : Int
    , errors : List String
    }


initialModel : Model
initialModel =
    { sequences = []
    , sequenceUploadFileName = Nothing
    , k = 0
    , errors = []
    }
