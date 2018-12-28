module Types exposing (AuthNStatus(..), Endpoint, Error, Flags, Jwk, Jwks, Jwt, Kid, Model, Msg(..), Nonce, OidcAuthFragment, Session, SignOutRequest, SigninResponse, State, VerifyableJwt)

import Browser exposing (..)
import Browser.Navigation exposing (Key)
import Json.Decode exposing (Value)
import Url exposing (Url)
import Route exposing (..)
import Appointment exposing (..)
import Set exposing (..)


type alias Model =
    { endpoint : Endpoint
    , navKey : Key
    , status : AuthNStatus
    , route : Route
    , hosts : List Host
    }


type alias Flags =
    { oidcEndpoint : Endpoint
    , token : Maybe Jwt
    }


type alias Endpoint =
    { clientId : String
    , authRedirect : String
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
    = NotSignedIn
    | Redirecting -- got nonce and state
    | Verifying OidcAuthFragment
    | SignedIn Jwt -- got verified token from js
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
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHosts (Result String (List Host))


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


