port module Port exposing (gotPortMsg, sendPortMsg)

import Json.Encode as JE


port sendPortMsg : JE.Value -> Cmd msg


port gotPortMsg : (JE.Value -> msg) -> Sub msg
