module Main exposing (..)

import Browser exposing (Document, document)
import Browser.Navigation exposing (load)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Url exposing (Protocol(..), Url)
import SignIn exposing (toAuthUrl, parseSignInStatus)
import Types exposing (..)

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

someState : String
someState = "SomeState"

someNonce : String
someNonce = "SomeNonce"

init : String -> ( Model, Cmd Msg )
init l =
    let
        s = parseSignInStatus l someState someNonce
    in 
        ( { status = s }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

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
                    , state = someState
                    , nonce = someNonce
                    }
            in
            ( model, toAuthUrl auth request |> Url.toString |> Browser.Navigation.load )

view : Model -> Document Msg
view m =
    { title = "OpenId Connect with okta and elm"
    , body = [ loginComponent m.status ] }

loginComponent : SignedInStatus -> Html Msg
loginComponent s =
    let
        notSignedIn = loginButton
        signedIn user = text <| String.concat ["You are signed in as ", user ]
        error = text "There was an error."
    in
        case s of
            Nothing -> notSignedIn
            Just (Ok username) -> signedIn username
            Just (Err _) -> error
    

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
