module SignIn exposing
    ( toAuthUrl
    , parseSignInStatus
    , parseFragment
    , readUserName
    , jwtFromToken
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
kvp k v = String.concat [ k, "=", v ]

queryString : List String -> String
queryString = String.concat << List.intersperse "&"

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
        , kvp "post_logout_redirect_uri" <| Url.toString s.redirect ]

parseSignInStatus : String -> String -> String -> Maybe (Result Error String)
parseSignInStatus location state nonce =
    case fromString location of
        Just url -> Maybe.map (signInUser state nonce) url.fragment
        _ -> Just (Err "Bad location")
        
signInUser : String -> String -> String -> Result Error String
signInUser state nonce f =
    parseFragment f
        |> Result.andThen (getJwt state)
        |> Result.andThen verifySignature
        |> Result.andThen readPayload
        |> Result.andThen (validateNonce nonce)

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

getJwt : String -> SigninFragment -> Result Error Jwt
getJwt ourState ( token, theirState ) =
    if ourState == theirState then
        jwtFromToken token

    else
        Err "Mismatching State"

jwtFromToken : String -> Result Error Jwt
jwtFromToken token =
    case String.split "." token of
        [ h, p, s ] ->
            Result.map3 Jwt (decode h) (decode p) (Ok s)

        _ ->
            Err "malformed jwt-token"

verifySignature : Jwt -> Result Error String
verifySignature jwt =
    Ok jwt.payload

readPayload : String -> Result Error Payload
readPayload =
    let
        nonce =
            field "nonce" string

        user =
            field "preferred_username" string

        withNonce =
            Json.Decode.map2 Payload user nonce
    in
    Result.mapError (always "Unexpected Json") << decodeString withNonce

validateNonce : String -> Payload -> Result Error String
validateNonce n p =
    if p.nonce == n then
        Ok p.userName

    else
        Err "Bad nonce."


readUserName : String -> String -> Result String String
readUserName ourNonce p =
    Result.andThen (validateNonce ourNonce) <|
        readPayload p






