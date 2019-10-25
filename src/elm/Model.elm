module Model exposing (Model, initialModel)

import DeBruijn exposing (Path)


type alias Model =
    { sequences : List String
    , sequenceUploadFileName : Maybe String
    , k : Int
    , paths : List Path
    , isGenerated : Bool
    , errors : List String
    }


initialModel : Model
initialModel =
    { sequences = []
    , sequenceUploadFileName = Nothing
    , k = 0
    , paths = []
    , isGenerated = False
    , errors = []
    }
