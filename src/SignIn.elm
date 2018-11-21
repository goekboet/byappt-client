module SignIn exposing
    ( assertNonce
    , getToken
    , parseFragment
    , toAuthUrl
    , toSignOutUrl
    )

import Base64 exposing (decode)
import Json.Decode exposing (decodeString, field, string)
import Types exposing (..)
import Url exposing (Url, fromString)


toAuthUrl : Url -> AuthRequest -> Url
toAuthUrl endpoint request =
    { endpoint | query = Just <| toAuthQueryString request }


toSignOutUrl : Url -> SignOut -> Url
toSignOutUrl endpoint req =
    { endpoint | query = Just <| toSignoutQueryString req }


kvp : String -> String -> String
kvp k v =
    String.concat [ k, "=", v ]


queryString : List String -> String
queryString =
    String.concat << List.intersperse "&"


toAuthQueryString : AuthRequest -> String
toAuthQueryString r =
    let
        wsSep =
            String.concat << List.intersperse " "
    in
    queryString
        [ kvp "client_id" r.clientId
        , kvp "redirect_uri" <| Url.toString r.redirectUri
        , kvp "response_type" <| wsSep r.responseType
        , kvp "scope" <| wsSep r.scope
        , kvp "state" r.state
        , kvp "nonce" r.nonce
        ]


toSignoutQueryString : SignOut -> String
toSignoutQueryString s =
    queryString
        [ kvp "id_token_hint" s.token
        , kvp "post_logout_redirect_uri" <| Url.toString s.redirect
        ]

type alias SigninFragment =
    ( String, String )

parseFragment : String -> Result Error SigninFragment
parseFragment f =
    let
        parse =
            List.map (String.split "=") << String.split "&"
    in
    case parse f of
        [ [ "id_token", a ], [ "state", b ] ] ->
            Ok ( a, b )

        [ [ "state", _ ], [ "error", msg ] ] ->
            Err msg

        _ ->
            Err "Bad fragment format"

getToken : String -> SigninFragment -> Result Error String
getToken state ( token, s ) =
    if s == state then
        Ok token

    else
        Err "Mismatching state"


assertNonce : String -> String -> Result Error String
assertNonce token nonce =
    case String.split "." token of
        [ h, p, s ] ->
            decode p
                |> Result.andThen readNonce
                |> Result.andThen (validateNonce nonce)
                |> Result.map (always token)

        _ ->
            Err "malformed jwt-token"


readNonce : String -> Result Error String
readNonce =
    let
        nonce =
            field "nonce" string
    in
    Result.mapError (always "Unexpected Json") << decodeString nonce


validateNonce : String -> String -> Result Error ()
validateNonce ourNonce theirNonce =
    if ourNonce == theirNonce then
        Ok ()

    else
        Err "Bad nonce."
