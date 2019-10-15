module Model exposing (Model, initialModel, resetError)


type alias Model =
    { sequences : List String
    , k : Int
    , readyToGenerate : Bool
    , dot : String
    , error : Bool
    , errorMessage : String
    }


initialModel : Model
initialModel =
    { sequences = []
    , k = 0
    , readyToGenerate = False
    , dot = "digraph  {A -> B}"
    , error = False
    , errorMessage = ""
    }


resetError : Model -> Model
resetError model =
    { model | error = False, errorMessage = "" }
