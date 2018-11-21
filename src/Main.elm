port module Main exposing (Flags, Handles, Model, Msg(..), auth, checkCallback, fromJson, init, loginComponent, main, myEndpoint, oktaEndpoint, rememberSession, sessionHandles, subscriptions, update, view)

import Browser exposing (Document, document)
import Browser.Navigation exposing (load)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode exposing (Value, decodeValue, field, string)
import SignIn exposing (getToken, parseFragment, toAuthUrl, toSignOutUrl, assertNonce )
import Types exposing (..)
import Url exposing (Protocol(..), Url)


port rememberSession : () -> Cmd msg


port sessionHandles : (Value -> msg) -> Sub msg


port gotToken : String -> Cmd msg


port forgetToken : () -> Cmd msg


main =
    document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { token : Maybe String
    , errors : List String
    }


type alias Handles =
    { state : String
    , nonce : String
    }


fromJson : Value -> Result Error Handles
fromJson =
    let
        decode =
            Json.Decode.map2 Handles (field "state" string) (field "state" string)
    in
    Result.mapError (always "Bad json sessionHandles") << decodeValue decode


type alias Flags =
    { location : String
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
                    ( { token = Just t, errors = [] }, gotToken t )

                Err m ->
                    ( { token = Nothing, errors = [ m ] }, Cmd.none )

        Nothing ->
            ( { token = flags.token, errors = [] }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    sessionHandles SignIn


type Msg
    = NoOp
    | RememberSession
    | SignIn Value
    | SignedIn String
    | SignOut String

signOut : String -> String
signOut t =
    let
        req = { redirect = myEndpoint
              , token = t 
              }
    in
        Url.toString <| toSignOutUrl signOutUrl req

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RememberSession ->
            ( model, rememberSession () )

        SignIn session ->
            case fromJson session of
                Ok handle ->
                    let
                        request =
                            { clientId = "0oahdv4gzpBZwPM6S0h7"
                            , redirectUri = myEndpoint
                            , responseType = [ "id_token" ]
                            , scope = [ "openid" ]
                            , state = handle.state
                            , nonce = handle.nonce
                            }
                    in
                    ( model, toAuthUrl auth request |> Url.toString |> Browser.Navigation.load )

                Err m ->
                    ( { model | errors = m :: model.errors }, Cmd.none )

        SignedIn token ->
            ( model, gotToken token )

        SignOut token ->
            ( { model | token = Nothing }
            , Cmd.batch
                [ forgetToken ()
                , signOut token |> Browser.Navigation.load
                ]
            )


view : Model -> Document Msg
view m =
    { title = "OpenId Connect with okta and elm"
    , body = [ loginComponent m.token ]
    }


loginComponent : Maybe String -> Html Msg
loginComponent s =
    case s of
        Nothing ->
            signInButton

        Just token ->
            signOutButton token


signInButton : Html Msg
signInButton =
    Html.button [ onClick <| RememberSession ] [ Html.text "Sign in" ]


signOutButton : String -> Html Msg
signOutButton token =
    Html.button [ onClick <| SignOut token ] [ Html.text "Sign out" ]


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
