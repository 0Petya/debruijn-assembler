module Model exposing (Model, initialModel)

import DeBruijn exposing (Graph)


type alias Model =
    { sequences : List String
    , sequenceUploadFileName : Maybe String
    , k : Int
    , graph : Graph
    , isGenerated : Bool
    , errors : List String
    }


initialModel : Model
initialModel =
    { sequences = []
    , sequenceUploadFileName = Nothing
    , k = 0
    , graph = { nodes = [], edges = [] }
    , isGenerated = False
    , errors = []
    }
