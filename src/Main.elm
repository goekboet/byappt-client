port module Main exposing (rememberSession, sessionHandles, gotToken, tokenVerified, forgetToken, forgotToken)

import Browser exposing (Document, document)
import Browser.Navigation exposing (load)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode exposing (Value, decodeValue, field, string)
import SignIn exposing (assertNonce, getToken, parseFragment, toAuthUrl, toSignOutUrl)
import Discovery exposing (Endpoint)
import Types exposing (..)
import Url exposing (Protocol(..), Url)
import Http exposing (get, expectJson)


port rememberSession : () -> Cmd msg


port sessionHandles : (Value -> msg) -> Sub msg


port gotToken : String -> Cmd msg


port tokenVerified : (Value -> msg) -> Sub msg


port forgetToken : () -> Cmd msg

port forgotToken : (String -> msg) -> Sub msg


main =
    document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type AuthNStatus
    = NotLoggedIn
    | Redirecting Value -- got nonce and state
    | LoggingIn String -- got token from redirect
    | LoggedIn String -- got verified token from js
    | SigningOut -- cleared token from js

type alias Model =
    { endpoint : Endpoint
    , status : Result String AuthNStatus 
    }


type alias Handles =
    { state : String
    , nonce : String
    }


fromJson : Value -> Result Error Handles
fromJson =
    let
        decode =
            Json.Decode.map2 Handles (field "state" string) (field "nonce" string)
    in
    Result.mapError (always "Bad json sessionHandles") << decodeValue decode


type alias Flags =
    { location : String
    , oidcEndpoint : Endpoint
    , state : Maybe String
    , nonce : Maybe String
    , token : Maybe String
    }


checkCallback : String -> String -> String -> Result Error String
checkCallback fragment state nonce =
    parseFragment fragment
        |> Result.andThen (getToken state)
        |> Result.andThen (assertNonce nonce)

init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        fragment =
            Maybe.andThen .fragment (Url.fromString flags.location)

        tkn =
            Maybe.map3 checkCallback fragment flags.state flags.nonce
    in
    case tkn of
        Just res ->
            case res of
                Ok t ->
                    ( 
                        { status = Ok (LoggingIn t)
                        , endpoint = flags.oidcEndpoint 
                        }
                        , gotToken t  
                    )

                Err e ->
                    ( 
                        { status = Err e
                        , endpoint = flags.oidcEndpoint
                        }
                        , Cmd.none 
                    )

        Nothing ->
            case flags.token of
                Just t ->
                    ( { status = Ok (LoggedIn t), endpoint = flags.oidcEndpoint }, Cmd.none )

                Nothing ->
                    ( { status = Ok NotLoggedIn, endpoint = flags.oidcEndpoint }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ sessionHandles GotSession
        , tokenVerified TokenVerified
        , forgotToken SignedOut]

type Msg
    = NoOp
    | RememberSession
    | GotSession Value
    | TokenVerified Value
    | SignedIn String
    | SignOut
    | SignedOut String


redirectToSignoutEndpoint : Endpoint -> String -> Result Error (Cmd msg)
redirectToSignoutEndpoint e t =
    let
        req =
            { redirect = myEndpoint
            , token = t
            }
    in
        Result.fromMaybe "Bad signout endpoint" (Url.fromString e.endSession)
            |> Result.map (\x -> toSignOutUrl x req)
            |> Result.map Url.toString
            |> Result.map load

redirectToAuthEndpoint : Endpoint -> Handles -> Result Error (Cmd msg)
redirectToAuthEndpoint e s =
    let
        request =
            { clientId = "0oahdv4gzpBZwPM6S0h7"
            , redirectUri = myEndpoint
            , responseType = [ "id_token" ]
            , scope = [ "openid" ]
            , state = s.state
            , nonce = s.nonce
            }
    in
        Result.fromMaybe "Bad auth endpoint" (Url.fromString e.auth)
            |> Result.map (\x -> toAuthUrl x request)
            |> Result.map Url.toString
            |> Result.map load


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RememberSession ->
            ( model , rememberSession () )

        GotSession s ->
            ( 
                { model 
                | status = Ok (Redirecting s) 
                }
                , 
                fromJson s
                    |> Result.andThen (redirectToAuthEndpoint model.endpoint)
                    |> Result.withDefault Cmd.none 
            )

        TokenVerified s ->
            case decodeValue string s of
                Ok t ->
                    ( { model | status = Ok (LoggedIn t) }, Cmd.none )

                _ ->
                    ( { model | status = Err "Verify jwt failed" }, Cmd.none )

        SignedIn token ->
            ( model, gotToken token )

        SignOut ->
            ( { model 
              | status = Ok SigningOut
              }
            , forgetToken ()
            )
        
        SignedOut t ->
            ( model
            , redirectToSignoutEndpoint model.endpoint t
                |> Result.withDefault Cmd.none
            )


view : Model -> Document Msg
view m =
    { title = "OpenId Connect with okta and elm"
    , body = [ loginComponent m.status ]
    }


loginComponent : Result String AuthNStatus -> Html Msg
loginComponent r =
    case r of
        Ok status ->
            signInStatus status

        Err msg ->
            Html.text msg


signInStatus : AuthNStatus -> Html Msg
signInStatus s =
    case s of
        NotLoggedIn ->
            signInButton

        LoggingIn _ ->
            Html.text "logging in..."

        LoggedIn t ->
            signOutButton t

        SigningOut ->
            Html.text "Logging out"

        Redirecting _ -> 
            Html.text "Redirecting."


signInButton : Html Msg
signInButton =
    Html.button [ onClick <| RememberSession ] [ Html.text "Sign in" ]


signOutButton : String -> Html Msg
signOutButton token =
    Html.button [ onClick SignOut ] [ Html.text "Sign out" ]


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


signOutUrl : Url
signOutUrl =
    { oktaEndpoint | path = "/oauth2/default/v1/logout" }
