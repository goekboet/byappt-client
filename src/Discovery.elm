module Discovery exposing (Endpoint, fromJson)

import Url exposing (Url)
import Json.Decode exposing (Decoder(..), field, string)

type alias Endpoint =
    { auth : String
    , endSession : String
    , keys : String
    }

fromJson : Decoder Endpoint
fromJson =
    let
        auth = field "authorization_endpoint" string
        signout = field "end_session_endpoint" string
        keys = field "jwks_uri" string
    in
        Json.Decode.map3 Endpoint auth signout keys