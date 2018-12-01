module Types exposing (AuthNStatus(..), Endpoint, Error, Flags, Jwk, Jwks, Jwt, Kid, Model, Msg(..), Nonce, OidcAuthFragment, Origin, Session, SignOutRequest, SigninResponse, State, VerifyableJwt)

import Json.Decode exposing (Value)
import Url exposing (Url)


type alias Model =
    { endpoint : Endpoint
    , origin : Url
    , status : Result String AuthNStatus
    }


type alias Origin =
    Url


type alias Flags =
    { origin : String
    , oidcEndpoint : Endpoint
    , token : Maybe Jwt
    }


type alias Endpoint =
    { clientId : String
    , auth : String
    , endSession : String
    , keys : String
    }


type alias SignOutRequest =
    { redirect : Url
    , token : Jwt
    }


type alias Error =
    String


type alias OidcAuthFragment =
    { idtoken : Jwt
    , state : State
    }


type AuthNStatus
    = NotLoggedIn
    | Redirecting -- got nonce and state
    | Verifying OidcAuthFragment
    | LoggedIn Jwt -- got verified token from js
    | SigningOut -- cleared token from js


type Msg
    = NoOp
    | RememberSession
    | SessionRemebered Session
    | SessionRecalled (Maybe Nonce)
    | GotKey (Result String VerifyableJwt)
    | TokenVerified Value
    | SignOut
    | SignedOut String


type alias State =
    String


type alias Nonce =
    String


type alias Session =
    { key : State
    , nonce : Nonce
    }


type alias Kid =
    String


type alias Jwt =
    String


type alias Jwk =
    Value


type alias Jwks =
    String


type alias SigninResponse =
    { kid : Kid
    , jwt : Jwt
    }


type alias VerifyableJwt =
    { kid : Kid
    , jwt : Jwt
    , jwk : Maybe Jwk
    }
