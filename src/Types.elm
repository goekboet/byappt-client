module Types exposing (..)

import Url exposing (Url)

type alias AuthRequest =
    { clientId : String
    , redirectUri : Url
    , responseType : List String
    , scope : List String
    , state : String
    , nonce : String
    }

type alias SignOut =
    { redirect : Url
    , token : String
    }

type alias Error =
    String

type alias SigninFragment =
    ( String, String )

type alias Jwt =
    { header : String
    , payload : String
    , signature : String
    }

type alias Payload =
    { userName : String, nonce : String }