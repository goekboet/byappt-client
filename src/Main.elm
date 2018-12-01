port module Main exposing (forgetToken, forgotToken, rememberSession, sessionRemebered, tokenVerified, verifyToken)

import Browser exposing (Document, document)
import Browser.Navigation exposing (load)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http as Http
import Json.Decode exposing (Value, decodeValue, field, string)
import SignIn exposing (..)
import Types exposing (..)
import Url exposing (Protocol(..), Url)


port rememberSession : () -> Cmd msg


port sessionRemebered : (Session -> msg) -> Sub msg


port recallSession : String -> Cmd msg


port sessionRecalled : (Maybe Nonce -> msg) -> Sub msg


port verifyToken : VerifyableJwt -> Cmd msg


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


makeKeyResponse : SigninResponse -> Bool -> Result Http.Error Jwks -> Msg
makeKeyResponse signin isRetry response =
    let
        notFound k = case k of
            Just x -> 
                Ok (Just x)
            Nothing -> 
                if isRetry 
                then Err "Key not found" 
                else Ok Nothing

        key =
            response
                |> Result.mapError (always "Request errored.")
                |> Result.andThen (getKey signin.kid)
                |> Result.andThen notFound
                |> Result.map (VerifyableJwt signin.kid signin.jwt)
    in
    GotKey key


fetchKey : Endpoint -> Bool -> SigninResponse -> Cmd Msg
fetchKey ept isRetry signin =
    let
        cacheControl = 
            if isRetry
            then [ Http.header "Cache-Control" "no-cache" ]
            else []

        handleResponse =
            makeKeyResponse signin isRetry
    in
    Http.request 
        { method = "GET"
        , headers = cacheControl
        , url = ept.keys
        , body = Http.emptyBody
        , expect = Http.expectString handleResponse 
        , timeout = Nothing
        , tracker = Nothing
        }

noUrl : Url
noUrl =
    { protocol = Url.Https
    , host = ""
    , port_ = Nothing
    , path = ""
    , query = Nothing
    , fragment = Nothing
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        origin =
            Url.fromString flags.origin

        status =
            initialAuthN flags
    in
    case ( origin, status ) of
        ( Nothing, _ ) ->
            ( { endpoint = flags.oidcEndpoint
              , origin = noUrl
              , status = Err "Bad origin"
              }
            , Cmd.none
            )

        ( Just o, Err e ) ->
            ( { endpoint = flags.oidcEndpoint
              , origin = o
              , status = Err e
              }
            , Cmd.none
            )

        ( Just o, Ok (LoggedIn t) ) ->
            ( { endpoint = flags.oidcEndpoint
              , origin = o
              , status = Ok (LoggedIn t)
              }
            , Cmd.none
            )

        ( Just o, Ok (Verifying fragment) ) ->
            ( { endpoint = flags.oidcEndpoint
              , origin = o
              , status = Ok (Verifying fragment)
              }
            , recallSession fragment.state
            )

        ( Just o, _ ) ->
            ( { endpoint = flags.oidcEndpoint
              , origin = o
              , status = Ok NotLoggedIn
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ sessionRemebered SessionRemebered
        , tokenVerified TokenVerified
        , forgotToken SignedOut
        , sessionRecalled SessionRecalled
        ]


redirectToSignoutEndpoint : Model -> Jwt -> Cmd msg
redirectToSignoutEndpoint m t =
    toSignOutUrl m.origin t m.endpoint
        |> load


redirectToAuthEndpoint : Model -> Session -> Cmd msg
redirectToAuthEndpoint m s =
    load (toAuthUrl m.origin m.endpoint s)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RememberSession ->
            ( model, rememberSession () )

        SessionRemebered s ->
            ( { model
                | status = Ok Redirecting
              }
            , redirectToAuthEndpoint model s
            )

        SessionRecalled n ->
            case ( n, model.status ) of
                ( Just nonce, Ok (Verifying f) ) ->
                    let
                        response =
                            assertNonce nonce f.idtoken

                        newStatus =
                            response
                                |> Result.map (always (Verifying f))
                    in
                    ( { model
                        | status = newStatus
                      }
                    , response
                        |> Result.map (fetchKey model.endpoint False)
                        |> Result.withDefault Cmd.none 
                    )

                _ ->
                    ( { model
                        | status = Err "Session not found"
                      }
                    , Cmd.none
                    )

        GotKey response ->
            case response of
                Ok v -> case v.jwk of
                    Just k -> ( model, verifyToken v )
                    Nothing -> ( model, fetchKey model.endpoint True (SigninResponse v.kid v.jwt))

                Err m ->
                    ( { model | status = Err m }, Cmd.none )

        TokenVerified s ->
            case decodeValue string s of
                Ok t ->
                    ( { model | status = Ok (LoggedIn t) }, Cmd.none )

                _ ->
                    ( { model | status = Err "Verify jwt failed" }, Cmd.none )

        SignOut ->
            ( { model
                | status = Ok SigningOut
              }
            , forgetToken ()
            )

        SignedOut t ->
            ( model
            , redirectToSignoutEndpoint model t
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

        Verifying _ ->
            Html.text "logging in..."

        LoggedIn t ->
            signOutButton t

        SigningOut ->
            Html.text "Logging out"

        Redirecting ->
            Html.text "Redirecting."


signInButton : Html Msg
signInButton =
    Html.button [ onClick <| RememberSession ] [ Html.text "Sign in" ]


signOutButton : String -> Html Msg
signOutButton token =
    Html.button [ onClick SignOut ] [ Html.text "Sign out" ]
