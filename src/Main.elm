port module Main exposing (forgetToken, forgotToken, rememberSession, sessionRemebered, tokenVerified, verifyToken)

import Browser exposing (..)
import Browser.Navigation as Nav
import Home exposing (..)
import Html as Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http as Http
import Json.Decode exposing (Value, decodeValue, field, string)
import SignIn exposing (..)
import Types exposing (..)
import Url as Url exposing (Protocol(..), Url)
import Route exposing (Route(..))


port rememberSession : () -> Cmd msg


port sessionRemebered : (Session -> msg) -> Sub msg


port recallSession : String -> Cmd msg


port sessionRecalled : (Maybe Nonce -> msg) -> Sub msg


port verifyToken : VerifyableJwt -> Cmd msg


port tokenVerified : (Value -> msg) -> Sub msg


port forgetToken : () -> Cmd msg


port forgotToken : (String -> msg) -> Sub msg


main =
    application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }


makeKeyResponse : SigninResponse -> Bool -> Result Http.Error Jwks -> Msg
makeKeyResponse signin isRetry response =
    let
        notFound k =
            case k of
                Just x ->
                    Ok (Just x)

                Nothing ->
                    if isRetry then
                        Err "Key not found"

                    else
                        Ok Nothing

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
            if isRetry then
                [ Http.header "Cache-Control" "no-cache" ]

            else
                []

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

stripFragment : Url -> String
stripFragment url =
    Url.toString { url | fragment = Nothing }

init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        signinFragment =
            parseInitialUrl url

        model =
            { endpoint = flags.oidcEndpoint
            , navKey = key
            , status = Ok NotSignedIn
            , route = Home
            }
    in
    case ( signinFragment, flags.token ) of
        ( Just (Ok fgmt), _ ) ->
            ( { model
                | status = Ok (Verifying fgmt)
              }
            , recallSession fgmt.state
            )

        ( Just (Err msg), _ ) ->
            ( { model
                | status = Err msg
              }
            , Nav.replaceUrl model.navKey (stripFragment url) 
            )

        ( Nothing, Just jwt ) ->
            ( { model
                | status = Ok (SignedIn jwt)
                , route = Route.toRoute url
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


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
    toSignOutUrl t m.endpoint
        |> Nav.load


redirectToAuthEndpoint : Model -> Session -> Cmd msg
redirectToAuthEndpoint m s =
    Nav.load (toAuthUrl m.endpoint s)


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
                Ok v ->
                    case v.jwk of
                        Just k ->
                            ( model, verifyToken v )

                        Nothing ->
                            ( model, fetchKey model.endpoint True (SigninResponse v.kid v.jwt) )

                Err m ->
                    ( { model | status = Err m }, Cmd.none )

        TokenVerified s ->
            case decodeValue string s of
                Ok t ->
                    ( { model | status = Ok (SignedIn t) }, Cmd.none )

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

        ChangedUrl url ->
            ( model, Cmd.none )

        ClickedLink req ->
            ( model, Cmd.none )


view : Model -> Document Msg
view m =
    { title = "OpenId Connect with okta and elm"
    , body = body <| index (Result.withDefault NotSignedIn m.status) 
    }

homeLink : Html Msg
homeLink =
    Html.a
        [ Attr.href "/" ]
        [ Html.h1
            [ Attr.class "homelink" ]
            [ Html.text "Byappt" ] 
        ]

body : Html Msg -> List (Html Msg)
body content =
    [ Html.div
        [ Attr.style "max-width" "32em"
        , Attr.style "margin" "auto"
        , Attr.style "padding-top" "1em"
        , Attr.style "padding-bottom" "1em"
        , Attr.style "background-color" "gainsboro"
        ]
        [ homeLink 
        , content 
        ]
    ]
