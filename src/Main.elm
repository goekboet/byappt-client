module Main exposing (..)

import Browser exposing (Document, document)
import Browser.Navigation exposing (load) 
import Url exposing (Url, Protocol(..))
import OAuth exposing (Token)
--import OAuth.Implicit exposing (Authorization, makeAuthUrl)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
    document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { error : Maybe String
    , token : Maybe Token
    }


type Msg
    = NoOp
    | SignIn


init : () -> ( Model, Cmd Msg )
init _ =
    ( { error = Nothing, token = Nothing }, Cmd.none )


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
        kvp k v = String.concat [k, "=", v]
        queryString = String.concat << List.intersperse "&"
        wsSep = String.concat << List.intersperse " "
    in
        String.cons '?' (queryString 
            [ (kvp "client_id" r.clientId) 
            , (kvp "redirect_uri" <| Url.toString r.redirectUri)
            , (kvp "response_type" <| wsSep r.responseType)
            , (kvp "scope" <| wsSep r.scope)
            , (kvp "state" r.state)
            , (kvp "nonce" r.nonce)
            ])

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> ( model, Cmd.none )
        SignIn -> 
            let
                redirect =
                        { clientId = clientId
                        , url = auth
                        , redirectUri = myEndpoint
                        , scope = ["openid"] 
                        , state = Just "state-296bc9a0-a2a2-4a57-be1a-d0e2fd9bb601"
                        }
            in
                (model, Cmd.none)
    


view : Model -> Document Msg
view _ =
    { title = "OpenId Connect with okta and elm", body = [ loginButton ] }

loginButton : Html Msg
loginButton = Html.button  [onClick SignIn] [Html.text "Sign in"]

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
auth = { oktaEndpoint | path = "/oauth2/default/v1/authorize" }

clientId : String
clientId = "0oahdv4gzpBZwPM6S0h7"

