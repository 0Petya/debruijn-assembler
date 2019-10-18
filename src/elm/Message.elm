module Message exposing (..)

import File exposing (File)


type Msg
    = SequenceInput String
    | SequenceUpload
    | SequenceSelected File
    | SequenceLoaded String
    | KInput String
    | Generate
