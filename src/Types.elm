module Types exposing (..)

import Url exposing (Url)
import Json.Decode exposing (Value)

type alias AuthRequest =
    { clientId : String
    , redirectUri : Url
    , responseType : List String
    , scope : List String
    , state : String
    , nonce : String
    }

type alias SignOutRequest =
    { redirect : Url
    , token : String
    }

type alias Error =
    String

type AuthNStatus
    = NotLoggedIn
    | Redirecting Value -- got nonce and state
    | Verifying 
    | LoggedIn String -- got verified token from js
    | SigningOut -- cleared token from js

type Msg
    = NoOp
    | RememberSession
    | GotSession Value
    | GotKey (Result String VerifyableJwt)
    | TokenVerified Value
    | SignOut
    | SignedOut String

type alias State = String

type alias Nonce = String

type alias Session =
    { state : State
    , nonce : Nonce
    }

type alias Kid = String

type alias Jwt = String

type alias Jwk = Value

type alias Jwks = String

type alias SigninResponse =
    { kid : Kid
    , jwt : Jwt
    }

type alias VerifyableJwt =
    { jwt : Jwt
    , jwk : Jwk
    }