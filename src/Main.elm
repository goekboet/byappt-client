port module Main exposing (forgetToken, forgotToken, rememberSession, sessionRemebered, tokenVerified, verifyToken)

import Browser exposing (..)
import Browser.Navigation as Nav
import Dict as Dict
import Html as Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event exposing (onClick)
import Http as Http
import Json.Decode exposing (Value, decodeValue, field, string)
import Appointment exposing (mockHosts, mockAppointments, Host, HostId)
import Route exposing (Route(..), toRoute)
import SignIn exposing (..)
import Types exposing (..)
import Url as Url exposing (Protocol(..), Url)
import Url.Builder as BuildUrl


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


route : Nav.Key -> Url -> Cmd Msg
route key url =
    let
        mapRootToHome =
            if url.path == "/" then
                hostsUrl

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


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        signinFragment = parseInitialUrl url
        
        next = toRoute url
        host = hostFromRoute next 

        model =
            { endpoint = flags.oidcEndpoint
            , navKey = key
            , status = Ok NotSignedIn
            , route = next
            , currentHost = host
            }
    in
    case ( signinFragment, flags.token ) of
        ( Just (Ok fgmt), _ ) ->
            ( { model
                | status = Ok (Verifying fgmt)
                , route = Hosts
              }
            , recallSession fgmt.state
            )

        ( Just (Err msg), _ ) ->
            ( { model
                | status = Err msg
              }
            , route key url
            )

        ( Nothing, Just jwt ) ->
            ( { model
                | status = Ok (SignedIn jwt)
              }
            , route key url
            )

        _ ->
            ( { model
                | route = Route.toRoute url
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
                    ( { model | status = Ok (SignedIn t) }, Nav.replaceUrl model.navKey hostsUrl )

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
            case req of
                Internal url ->
                    let
                        next = toRoute url
                        host = hostFromRoute next 
                    in
                    ( { model
                        | route = next
                        , currentHost = host
                      }
                    , Nav.pushUrl model.navKey <| Url.toString url
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

hostFromRoute : Route -> Maybe Host
hostFromRoute r = case r of
    Appointments id -> getHost id mockHosts
    _ -> Nothing


getHost : HostId -> List Host -> Maybe Host
getHost id hosts = case List.filter (\h -> h.id == id) hosts of
    [ host ] -> Just host
    _ -> Nothing

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
        , Html.li [] [ signInLink model]
        ]

hostsUrl : String
hostsUrl =
    BuildUrl.absolute [ "hosts" ] []


hostsLink : Model -> List (Html Msg)
hostsLink model =
    let
        hostName = Maybe.map .name model.currentHost
            |> Maybe.map (\h -> Html.i [] [Html.text h])
            |> Maybe.withDefault (Html.text "")

    in
    [ Html.a
        [ Attr.href hostsUrl ]
        [ Html.text "Hosts" 
        ]
     , hostName ]


bookingsUrl : String
bookingsUrl =
    BuildUrl.absolute [ "bookings" ] []


myAppointmentsLink : Model -> Html Msg
myAppointmentsLink model =
    case Result.withDefault NotSignedIn model.status of
        SignedIn _ ->
            Html.a
                [ Attr.href bookingsUrl ]
                [ Html.text "Bookings" ]

        _ ->
            Html.a
                [ Attr.disabled True
                , Attr.title "Sign in to access Bookings"]
                [ Html.text "Bookings"]


content : Model -> List (Html Msg)
content model =
    case model.route of
        Hosts ->
            hostsTable

        Appointments hostId ->
            appointmentsTable hostId

        MyBookings ->
            [ Html.h2 [] [ Html.text "My bookings" ] ]

        Error msg ->
            errorPage msg

        _ ->
            [ Html.p [] [ Html.text "Not found" ] ]


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


hostsTable : List (Html Msg)
hostsTable =
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
                mockHosts
            )
        ]
    ]


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
    case Result.withDefault NotSignedIn model.status of
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
