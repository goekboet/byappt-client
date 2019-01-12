module SignIn exposing
    ( Endpoint
    , Jwks
    , Jwt
    , Kid
    , Nonce
    , OidcAuthFragment
    , Session
    , SigninResponse
    , VerifyableJwt
    , toSigninResponse
    , getKey
    , getValue
    , parseInitialUrl
    , toAuthUrl
    , toSignOutUrl
    )

import Base64 as Base64
import Http as Http exposing (Error)
import Json.Decode as Json exposing (Decoder, Value)
import Url as Url exposing (Url)
import Url.Builder as ToUrl
import Url.Parser as Parse exposing (Parser)


type alias Endpoint =
    { clientId : String
    , authRedirect : String
    , auth : String
    , endSession : String
    , keys : String
    }


type alias OidcAuthFragment =
    { idtoken : Jwt
    , state : State
    }


type alias Session =
    { key : State
    , nonce : Nonce
    }


type alias State =
    String


type alias Nonce =
    String


type alias Kid =
    String


type alias Jwk =
    Value


type alias VerifyableJwt =
    { kid : Kid
    , jwt : Jwt
    , jwk : Maybe Jwk
    }


type alias SignOutRequest =
    { redirect : Url
    , token : Jwt
    }


type alias Jwks =
    String


type alias SigninResponse =
    { kid : Kid
    , jwt : Jwt
    }


type alias Jwt =
    String


kvp : String -> String -> String
kvp k v =
    String.concat [ k, "=", v ]


queryString : List String -> String
queryString =
    String.concat << List.intersperse "&"


toAuthUrl : Endpoint -> Session -> String
toAuthUrl ept s =
    let
        query =
            [ ToUrl.string "client_id" ept.clientId
            , ToUrl.string "redirect_uri" ept.authRedirect
            , ToUrl.string "response_type" "token"
            , ToUrl.string "scope" "openid"
            , ToUrl.string "state" s.key
            , ToUrl.string "nonce" s.nonce
            ]
    in
    ToUrl.crossOrigin ept.auth [] query


toSignOutUrl : Jwt -> Endpoint -> String
toSignOutUrl token ept =
    let
        query =
            [ ToUrl.string "id_token_hint" token
            , ToUrl.string "post_logout_redirect_uri" ept.authRedirect
            ]
    in
    ToUrl.crossOrigin ept.endSession [] query



-- If the app was requested through a redirect from the
-- oidc provider there will be a fragment in the url that
-- is either a signinfragment or an error. This function parses
-- that fragment to an appropriate route


toAuthFragment : Maybe String -> Maybe (Result String OidcAuthFragment)
toAuthFragment fgmt =
    case fgmt of
        Just f ->
            parseSigninFragment f

        _ ->
            Nothing


parseQueryString : String -> List (List String)
parseQueryString =
    List.map (String.split "=") << String.split "&"


parseSigninFragment : String -> Maybe (Result String OidcAuthFragment)
parseSigninFragment f =
    case parseQueryString f of
        [ [ "access_token", a ], [ "token_type", "Bearer" ], [ "expires_in", _ ], [ "scope", "openid" ], [ "state", b ] ] ->
            Just (Ok <| OidcAuthFragment a b)

        [ [ "state", _ ], [ "error", e ], [ "error_description", desc ] ] ->
            Just (Err <| String.join " " [ e, desc ])

        _ ->
            Nothing


parseInitialUrl : Url -> Maybe (Result String OidcAuthFragment)
parseInitialUrl url =
    case Parse.parse (Parse.fragment toAuthFragment) url of
        Just x ->
            x

        _ ->
            Nothing


getToken : State -> OidcAuthFragment -> Result String Jwt
getToken s f =
    if s == f.state then
        Ok f.idtoken

    else
        Err "Mismatching state"

toSigninResponse : Jwt -> Result String SigninResponse
toSigninResponse token = 
    case String.split "." token of
        [ h, p, s ] ->
            let
                getKeyId =
                    Base64.decode h
                        |> Result.andThen readKid
            in
            Result.map2 SigninResponse getKeyId (Ok token)

        _ ->
            Err (String.join " " [ "malformed jwt-token:", token ])

assertNonce : Nonce -> Jwt -> Result String SigninResponse
assertNonce nonce token =
    case String.split "." token of
        [ h, p, s ] ->
            let
                checkNonce =
                    Base64.decode p
                        |> Result.andThen readNonce
                        |> Result.andThen (validateNonce nonce)
                        |> Result.map (always token)

                getKeyId =
                    Base64.decode h
                        |> Result.andThen readKid
            in
            Result.map2 SigninResponse getKeyId checkNonce

        _ ->
            Err (String.join " " [ "malformed jwt-token:", token ])


readNonce : String -> Result String String
readNonce =
    let
        nonce =
            Json.field "nonce" Json.string
    in
    Result.mapError (always "Unexpected Json") << Json.decodeString nonce


readKid : String -> Result String String
readKid =
    let
        kid =
            Json.field "kid" Json.string
    in
    Result.mapError (always "Unexpected json") << Json.decodeString kid


validateNonce : String -> String -> Result String ()
validateNonce ourNonce theirNonce =
    if ourNonce == theirNonce then
        Ok ()

    else
        Err "Bad nonce."


keysfield : Decoder (List Json.Value)
keysfield =
    Json.field "keys" (Json.list Json.value)


matchKid : Kid -> Jwk -> Bool
matchKid kid val =
    let
        kidfield =
            Json.field "kid" Json.string
    in
    Json.decodeValue kidfield val
        |> Result.map (\x -> x == kid)
        |> Result.withDefault False


getValue : Jwk -> Result String String
getValue val =
    let
        valuefield =
            Json.field "n" Json.string
    in
    Json.decodeValue valuefield val
        |> Result.mapError (always "malformed value")


getKey : Kid -> Jwks -> Result String (Maybe Jwk)
getKey kid keyset =
    Json.decodeString keysfield keyset
        |> Result.map (List.filter (matchKid kid))
        |> Result.mapError (always "malformed input")
        |> Result.map List.head
