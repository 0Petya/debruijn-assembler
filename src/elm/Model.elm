module Model exposing (Model, initialModel)

import DeBruijn exposing (Graph, Path)
import Dict


type alias Model =
    { sequences : List String
    , sequenceUploadFileName : Maybe String
    , k : Int
    , graph : Graph
    , paths : List Path
    , currentPath : Path
    , isGenerated : Bool
    , isCompressed : Bool
    , errors : List String
    }


initialModel : Model
initialModel =
    { sequences = []
    , sequenceUploadFileName = Nothing
    , k = 0
    , graph = Dict.empty
    , paths = []
    , currentPath = []
    , isGenerated = False
    , isCompressed = False
    , errors = []
    }
