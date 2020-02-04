module Message exposing (Msg(..))

import DeBruijn exposing (Path)
import File exposing (File)


type Msg
    = SequenceInput String
    | SequenceUpload
    | SequenceSelected File
    | SequenceLoaded String
    | KInput String
    | Generate
    | Compress
    | ViewPath Path
    | CutRepeats
