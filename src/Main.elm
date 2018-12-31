port module Main exposing (
    forgetToken, 
    forgotToken, 
    rememberSession, 
    sessionRemebered, 
    tokenVerified, 
    verifyToken,
    Route(..),
    toRoute)

import Browser exposing (..)
import Browser.Navigation as Nav
import Dict as Dict
import Host as Host exposing (mockAppointments, Host, HostId)
import Html as Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event exposing (onClick)
import Http as Http
import Json.Decode exposing (Value, decodeValue, field, string)
import Url.Parser as FromUrl exposing (Parser, (</>))
import Url.Parser.Query as Query
import Set as Set
import SignIn as S exposing (..)
import Url as Url exposing (Protocol(..), Url)
import Url.Builder as BuildUrl


type alias Model =
    { endpoint : Endpoint
    , navKey : Nav.Key
    , status : AuthNStatus
    , route : Route
    , hosts : List Host
    }


type Msg
    = NoOp
    | RememberSession
    | SessionRemebered Session
    | SessionRecalled (Maybe Nonce)
    | GotKey (Result String VerifyableJwt)
    | TokenVerified Value
    | SignOut
    | SignedOut String
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHosts (Result String (List Host))


type alias Flags =
    { oidcEndpoint : Endpoint
    , token : Maybe Jwt
    }


type AuthNStatus
    = NotSignedIn
    | Redirecting -- got nonce and state
    | Verifying OidcAuthFragment
    | SignedIn Jwt -- got verified token from js
    | SigningOut -- cleared token from js


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


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        signinFragment =
            S.parseInitialUrl url

        next =
            toRoute url

        model =
            { endpoint = flags.oidcEndpoint
            , navKey = key
            , status = NotSignedIn
            , route = next
            , hosts = []
            }
    in
    case ( signinFragment, flags.token ) of
        ( Just (Ok fgmt), _ ) ->
            ( { model
                | status = Verifying fgmt
                , route = Hosts
              }
            , recallSession fgmt.state
            )

        ( Just (Err msg), _ ) ->
            ( model
            , routeToError model msg
            )

        ( Nothing, Just jwt ) ->
            ( { model
                | status = SignedIn jwt
              }
            , route key url
            )

        _ ->
            ( { model
                | route = toRoute url
              }
            , route key url
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ sessionRemebered SessionRemebered
        , tokenVerified TokenVerified
        , forgotToken SignedOut
        , sessionRecalled SessionRecalled
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RememberSession ->
            ( model, rememberSession () )

        SessionRemebered s ->
            ( { model
                | status = Redirecting
              }
            , redirectToAuthEndpoint model s
            )

        SessionRecalled n ->
            case ( n, model.status ) of
                ( Just nonce, Verifying f ) ->
                    case S.assertNonce nonce f.idtoken of
                        Ok r ->
                            ( model, fetchKey model.endpoint False r )

                        Err m ->
                            ( { model | status = NotSignedIn }, routeToError model m )

                _ ->
                    ( { model | status = NotSignedIn }, routeToError model "No nonce in localstorage." )

        GotKey response ->
            case response of
                Ok v ->
                    case v.jwk of
                        Just k ->
                            ( model, verifyToken v )

                        Nothing ->
                            ( model, fetchKey model.endpoint True (S.SigninResponse v.kid v.jwt) )

                Err m ->
                    ( { model | status = NotSignedIn }, routeToError model m )

        TokenVerified s ->
            case decodeValue string s of
                Ok t ->
                    ( { model | status = SignedIn t }, Nav.replaceUrl model.navKey (BuildUrl.absolute [ "hosts" ] []) )

                _ ->
                    ( { model | status = NotSignedIn }, routeToError model "Bad jwt" )

        SignOut ->
            ( { model
                | status = SigningOut
              }
            , forgetToken ()
            )

        SignedOut t ->
            ( model
            , redirectToSignoutEndpoint model t
            )

        ChangedUrl url ->
            ( model
            , Cmd.batch
                [ fetchData model.route
                ]
            )

        ClickedLink req ->
            case req of
                Internal url ->
                    let
                        next =
                            toRoute url
                    in
                    ( { model
                        | route = next
                      }
                    , Cmd.batch
                        [ Nav.pushUrl model.navKey <| Url.toString url ]
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        GotHosts r ->
            gotHostData model r



-- Oidc Sign-in


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


redirectToSignoutEndpoint : Model -> Jwt -> Cmd msg
redirectToSignoutEndpoint m t =
    toSignOutUrl t m.endpoint
        |> Nav.load



-- Routing


type Route
    = Hosts
    | Appointments HostId
    | MyBookings
    | NotFound
    | Error String


parser : Parser (Route -> a) a
parser =
    let
        msg =
            Query.string "msg"
                |> FromUrl.query

        toError =
            Maybe.withDefault NotFound
                << Maybe.map Error
    in
    FromUrl.oneOf
        [ FromUrl.map Hosts (FromUrl.s "hosts")
        , FromUrl.map Appointments (FromUrl.s "hosts" </> FromUrl.string </> FromUrl.s "appointments")
        , FromUrl.map MyBookings (FromUrl.s "bookings")
        , FromUrl.map toError (FromUrl.s "error" </> msg)
        ]


toRoute : Url -> Route
toRoute url =
    Maybe.withDefault NotFound (FromUrl.parse parser url)


redirectToAuthEndpoint : Model -> Session -> Cmd msg
redirectToAuthEndpoint m s =
    Nav.load (S.toAuthUrl m.endpoint s)


switchToRoute : Model -> Url -> ( Model, Cmd Msg )
switchToRoute model url =
    let
        next =
            toRoute url
    in
    ( { model
        | route = next
      }
    , Cmd.batch
        [ Nav.pushUrl model.navKey <| Url.toString url ]
    )


route : Nav.Key -> Url -> Cmd Msg
route key url =
    let
        mapRootToHome =
            if url.path == "/" then
                BuildUrl.absolute [ "hosts" ] []

            else
                url.path

        mapped =
            Url.toString
                { url
                    | fragment = Nothing
                    , path = mapRootToHome
                }
    in
    Nav.pushUrl key mapped


hostsReq : Cmd Msg
hostsReq =
    let
        r fromApi =
            GotHosts <| Result.mapError (always "httperror") fromApi
    in
    Http.get
        { url = "https://local.byappt/api/hosts"
        , expect = Http.expectJson r Host.readHostResponse
        }



-- Hosts --


hostFromRoute : Route -> Maybe HostId
hostFromRoute r =
    case r of
        Appointments id ->
            Just id

        _ ->
            Nothing


fetchData : Route -> Cmd Msg
fetchData r =
    case r of
        Hosts ->
            hostsReq

        _ ->
            Cmd.none


getHost : HostId -> List Host -> Maybe Host
getHost id hosts =
    case List.filter (\h -> h.id == id) hosts of
        [ host ] ->
            Just host

        _ ->
            Nothing


gotHostData : Model -> Result String (List Host) -> ( Model, Cmd Msg )
gotHostData m res =
    case res of
        Err msg ->
            ( m, routeToError m msg )

        Ok hs ->
            ( { m | hosts = hs }, Cmd.none )


hostsTable : Model -> List (Html Msg)
hostsTable m =
    [ Html.h2 [] [ Html.text "Hosts" ]
    , Html.table []
        [ Html.thead []
            [ Html.tr []
                [ Html.th [] [ Html.text "Name" ] ]
            ]
        , Html.tbody []
            (List.map
                (\x ->
                    Html.tr []
                        [ Html.td []
                            [ Html.a
                                [ Attr.href (appointmentUrl x.id) ]
                                [ Html.text x.name ]
                            ]
                        ]
                )
                m.hosts
            )
        ]
    ]


view : Model -> Document Msg
view m =
    { title = "ByAppt"
    , body = [ grid m ]
    }


grid : Model -> Html Msg
grid model =
    Html.div
        [ Attr.class "grid" ]
        [ Html.div [ Attr.class "header" ] [ header model ]
        , Html.div [ Attr.class "sideBar" ] [ sideBar model ]
        , Html.div [ Attr.class "content" ] (content model)
        ]


header : Model -> Html Msg
header model =
    Html.h1 [] [ Html.text "Byappt" ]


sideBar : Model -> Html Msg
sideBar model =
    Html.ul
        []
        [ Html.li [] (hostsLink model)
        , Html.li [] [ myAppointmentsLink model ]
        , Html.li [] [ signInLink model ]
        ]



-- hostsUrl : String
-- hostsUrl =
--     BuildUrl.absolute [ "hosts" ] []


getHostFrom : Model -> Maybe Host
getHostFrom model =
    let
        hostId =
            hostFromRoute model.route

        findHost id =
            List.filter (\h -> h.id == id) model.hosts

        host =
            Maybe.map findHost hostId
    in
    case host of
        Just [ h ] ->
            Just h

        _ ->
            Nothing


hostsLink : Model -> List (Html Msg)
hostsLink model =
    let
        hostName =
            Maybe.map .name (getHostFrom model)
                |> Maybe.map (\h -> Html.i [] [ Html.text h ])
                |> Maybe.withDefault (Html.text "")
    in
    [ Html.a
        [ Attr.href <| BuildUrl.absolute [ "hosts" ] [] ]
        [ Html.text "Hosts"
        ]
    , hostName
    ]


bookingsUrl : String
bookingsUrl =
    BuildUrl.absolute [ "bookings" ] []


myAppointmentsLink : Model -> Html Msg
myAppointmentsLink model =
    case model.status of
        SignedIn _ ->
            Html.a
                [ Attr.href bookingsUrl ]
                [ Html.text "Bookings" ]

        _ ->
            Html.a
                [ Attr.disabled True
                , Attr.title "Sign in to access Bookings"
                ]
                [ Html.text "Bookings" ]


content : Model -> List (Html Msg)
content model =
    case model.route of
        Hosts ->
            hostsTable model

        Appointments hostId ->
            appointmentsTable hostId

        MyBookings ->
            [ Html.h2 [] [ Html.text "My bookings" ] ]

        Error msg ->
            errorPage msg

        _ ->
            [ Html.p [] [ Html.text "Not found" ] ]


errorUrl : String -> String
errorUrl msg =
    BuildUrl.absolute [ "error" ] [ BuildUrl.string "msg" msg ]


routeToError : Model -> String -> Cmd Msg
routeToError m msg =
    Nav.pushUrl m.navKey <| errorUrl msg


errorPage : String -> List (Html Msg)
errorPage msg =
    [ Html.h2 [] [ Html.text "Error" ]
    , Html.p [] [ Html.text msg ]
    ]


appointmentUrl : HostId -> String
appointmentUrl hostId =
    BuildUrl.relative
        [ "hosts"
        , hostId
        , "appointments"
        ]
        []


appointmentsTable : HostId -> List (Html Msg)
appointmentsTable hostId =
    let
        appts =
            Dict.get hostId mockAppointments
                |> Maybe.withDefault []
    in
    [ Html.h2 [] [ Html.text "Availiable appointments:" ]
    , Html.table []
        [ Html.thead []
            [ Html.tr []
                [ Html.th [] [ Html.text "Start" ]
                , Html.th [] [ Html.text "Duration" ]
                , Html.th [] [ Html.text "Book" ]
                ]
            ]
        , Html.tbody []
            (List.map
                (\x ->
                    Html.tr []
                        [ Html.td [] [ Html.text x.start ]
                        , Html.td [] [ Html.text (String.fromInt x.duration) ]
                        , Html.td [] [ Html.button [] [ Html.text "Book" ] ]
                        ]
                )
                appts
            )
        ]
    ]


signInLink : Model -> Html Msg
signInLink model =
    case model.status of
        NotSignedIn ->
            Html.a
                [ Event.onClick RememberSession ]
                [ Html.text "Sign in" ]

        SignedIn _ ->
            Html.a
                [ Event.onClick SignOut ]
                [ Html.text "Sign out" ]

        SigningOut ->
            Html.a
                []
                [ Html.text "Signing out" ]

        _ ->
            Html.a
                []
                [ Html.text "Signing in" ]
