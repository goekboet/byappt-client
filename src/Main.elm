module Main exposing (AuthRequest, Error, Jwt, Model, Msg(..), SignedInStatus, SigninFragment, auth, getJwt, init, jwtFromToken, loginButton, main, myEndpoint, oktaEndpoint, parseSinginFragment, readUserName, subscriptions, toAuthUrl, toQueryString, update, verifySignature, view)

--import OAuth.Implicit exposing (Authorization, makeAuthUrl)

import Base64 exposing (decode)
import Browser exposing (Document, document)
import Browser.Navigation exposing (load)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode exposing (decodeString, field, string)
import OAuth exposing (Token)
import Url exposing (Protocol(..), Url)


main =
    document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias SignedInStatus =
    Maybe (Result String String)


type alias Model =
    { status : SignedInStatus
    }


type Msg
    = NoOp
    | SignIn


init : () -> ( Model, Cmd Msg )
init _ =
    ( { status = Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type alias AuthRequest =
    { clientId : String
    , redirectUri : Url
    , responseType : List String
    , scope : List String
    , state : String
    , nonce : String
    }


toQueryString : AuthRequest -> String
toQueryString r =
    let
        kvp k v =
            String.concat [ k, "=", v ]

        queryString =
            String.concat << List.intersperse "&"

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


toAuthUrl : Url -> AuthRequest -> Url
toAuthUrl endpoint request =
    { endpoint | query = Just <| toQueryString request }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SignIn ->
            let
                request =
                    { clientId = "0oahdv4gzpBZwPM6S0h7"
                    , redirectUri = myEndpoint
                    , responseType = [ "id_token" ]
                    , scope = [ "openid", "profile" ]
                    , state = "someState"
                    , nonce = "someNonce"
                    }
            in
            ( model, toAuthUrl auth request |> Url.toString |> Browser.Navigation.load )


type alias Error =
    String


type alias SigninFragment =
    ( String, String )


type alias Jwt =
    { header : String
    , payload : String
    , signature : String
    }


parseSinginFragment : String -> Result Error SigninFragment
parseSinginFragment f =
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


jwtFromToken : String -> Result Error Jwt
jwtFromToken token =
    case String.split "." token of
        [ h, p, s ] ->
            Result.map3 Jwt (decode h) (decode p) (Ok s)

        _ ->
            Err "malformed jwt-token"


getJwt : String -> SigninFragment -> Result Error Jwt
getJwt ourState ( token, theirState ) =
    if ourState == theirState then
        jwtFromToken token

    else
        Err "Mismatching State"


verifySignature : Jwt -> Result Error String
verifySignature jwt =
    Ok jwt.payload


type alias Payload =
    { userName : String, nonce : String }


validateNonce : String -> Payload -> Result Error String
validateNonce n p =
    if p.nonce == n then
        Ok p.userName

    else
        Err "Bad nonce."


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


readUserName : String -> String -> Result String String
readUserName ourNonce p =
    Result.andThen (validateNonce ourNonce) <|
        readPayload p


view : Model -> Document Msg
view _ =
    { title = "OpenId Connect with okta and elm", body = [ loginButton ] }


loginButton : Html Msg
loginButton =
    Html.button [ onClick SignIn ] [ Html.text "Sign in" ]


myEndpoint : Url
myEndpoint =
    { protocol = Url.Https
    , host = "local.byappt"
    , port_ = Nothing
    , path = ""
    , query = Nothing
    , fragment = Nothing
    }


oktaEndpoint : Url
oktaEndpoint =
    { protocol = Url.Https
    , host = "dev-987804.oktapreview.com"
    , port_ = Nothing
    , path = ""
    , query = Nothing
    , fragment = Nothing
    }


auth : Url
auth =
    { oktaEndpoint | path = "/oauth2/default/v1/authorize" }
