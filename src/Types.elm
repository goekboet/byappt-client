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
