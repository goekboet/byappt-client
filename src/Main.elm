port module Main exposing
    ( Route(..)
    , cacheAuthFgmt
    , getState
    , gotState
    , setState
    , stateSet
    , toRoute
    )

import Browser exposing (..)
import Browser.Navigation as Nav
import Dict as Dict
import Host as Host exposing (Appointment, HostId, Hosts)
import Html as Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event exposing (onClick)
import Http as Http
import Json.Decode as D exposing (Value, decodeValue, field, string)
import Json.Encode as E
import SignIn as S exposing (BookingsAuth, Endpoint)
import Url as Url exposing (Protocol(..), Url)
import Url.Builder as BuildUrl
import Url.Parser as FromUrl exposing ((</>), Parser)
import Url.Parser.Query as Query


type alias Model =
    { endpoint : Endpoint
    , navKey : Nav.Key
    , error : List Error
    , bookingsAuth : Maybe BookingsAuth
    , route : Route
    , hosts : Maybe Hosts
    }


type Error
    = Message String
    | DecodeError D.Error
    | HttpError Http.Error


type alias Flags =
    { oidcEndpoint : Endpoint
    , bookingsAuth : Maybe String
    }


port setState : Value -> Cmd msg


port stateSet : (String -> msg) -> Sub msg


port getState : String -> Cmd msg


port gotState : (Maybe String -> msg) -> Sub msg


port cacheAuthFgmt : Value -> Cmd msg


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
        initialAuth =
            flags.bookingsAuth
                |> Maybe.map (D.decodeString S.bookingsAuthFromJson)

        bookingsAuth =
            url.fragment
                |> Maybe.andThen S.parseBookingsAuth

        url_ =
            if url.path == "/" then
                { url | path = "/hosts" }

            else
                url

        next =
            toRoute url_

        model =
            { endpoint = flags.oidcEndpoint
            , navKey = key
            , error = []
            , bookingsAuth = Nothing
            , route = next
            , hosts = Nothing
            }
    in
    case ( bookingsAuth, initialAuth ) of
        ( Just (Ok fgmt), _ ) ->
            ( { model
                | bookingsAuth = Just fgmt
              }
            , getState fgmt.state
            )

        ( Just (Err msg), _ ) ->
            ( { model | error = Message msg :: model.error }
            , Cmd.none
            )

        ( Nothing, Just (Ok auth) ) ->
            ( { model
                | bookingsAuth = Just auth
              }
            , route key url
            )

        ( _, Just (Err e) ) ->
            ( { model
                | error = DecodeError e :: model.error
              }
            , Cmd.none
            )

        _ ->
            switchToRoute model url_


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch
        [ stateSet StateSet
        , gotState (fromPortBookingState m)
        ]


type Msg
    = NoOp
    | Fail Error
    | StateSet String
    | GotState (Result Error BookingState)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHosts (Result String Hosts)
    | GotAppointments HostId (Result Error (List Appointment))
    | GotBookings (Result Error (List Appointment))
    | Book Appointment
    | BookingConfirm (Result Error Appointment)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        Fail e ->
            ( { model
                | error = e :: model.error
                , bookingsAuth = Nothing
              }
            , Cmd.none
            )

        StateSet s ->
            ( model
            , redirectToAuthEndpoint model s
            )

        GotState s ->
            case s of
                Ok state ->
                    routeFromBookingState model state

                Err m ->
                    ( { model | error = m :: model.error }
                    , Cmd.none
                    )

        ChangedUrl url ->
            let
                model_ =
                    { model
                        | route = toRoute url
                    }
            in
            ( model_
            , fetchData model_
            )

        ClickedLink req ->
            case req of
                Internal url ->
                    ( model, Nav.pushUrl model.navKey <| Url.toString url )

                External url ->
                    ( model
                    , Nav.load url
                    )

        GotHosts r ->
            gotHostData model r

        GotAppointments hostId r ->
            gotAppointmentsData model hostId r

        GotBookings bs ->
            gotBookingsData model bs

        Book appt ->
            case model.bookingsAuth of
                Just auth ->
                    ( model
                    , bookingsPost auth.accesstoken appt
                    )

                _ ->
                    ( model
                    , setState
                        (toJsonBookingState
                            { appt = Just appt
                            , url =
                                BuildUrl.absolute
                                    [ "hosts"
                                    , appt.hostId
                                    , "appointments"
                                    ]
                                    []
                            }
                        )
                    )

        BookingConfirm c ->
            ( model, Cmd.none )



-- Routing


type Route
    = Hosts
    | Appointments HostId (List Appointment)
    | MyBookings (List Appointment)
    | NotFound
    | Error String


toRoute : Url -> Route
toRoute url =
    Maybe.withDefault NotFound (FromUrl.parse parser url)


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
        , FromUrl.map (\x -> Appointments x []) (FromUrl.s "hosts" </> FromUrl.string </> FromUrl.s "appointments")
        , FromUrl.map (MyBookings []) (FromUrl.s "bookings")
        , FromUrl.map toError (FromUrl.s "error" </> msg)
        ]


type alias BookingState =
    { url : String
    , appt : Maybe Appointment
    }


toJsonBookingState : BookingState -> Value
toJsonBookingState s =
    let
        appt =
            case s.appt of
                Just a ->
                    E.object
                        [ ( "hostId", E.string a.hostId )
                        , ( "start", E.int a.start )
                        , ( "duration", E.int a.duration )
                        ]

                _ ->
                    E.null
    in
    E.object
        [ ( "url", E.string s.url )
        , ( "appt", appt )
        ]


fromJsonBookingState : D.Decoder BookingState
fromJsonBookingState =
    let
        appt =
            D.map3 Appointment
                (D.field "hostId" D.string)
                (D.field "start" D.int)
                (D.field "duration" D.int)
    in
    D.map2 BookingState
        (D.field "url" D.string)
        (D.field "appt" (D.nullable appt))


fromPortBookingState : Model -> Maybe String -> Msg
fromPortBookingState m s =
    case s of
        Just json ->
            D.decodeString fromJsonBookingState json
                |> Result.mapError DecodeError
                |> GotState

        Nothing ->
            Fail (Message "No matching state")


redirectToAuthEndpoint : Model -> String -> Cmd msg
redirectToAuthEndpoint m s =
    Nav.load (S.toAuthUrl m.endpoint s)


routeFromBookingState : Model -> BookingState -> ( Model, Cmd Msg )
routeFromBookingState m s =
    let
        next =
            Url.fromString s.url
                |> Maybe.map toRoute
                |> Maybe.withDefault NotFound
    in
    case ( s.appt, m.bookingsAuth ) of
        ( Just a, Just fgmt ) ->
            ( { m
                | route = next
              }
            , Cmd.batch
                [ Nav.pushUrl m.navKey s.url
                , bookingsPost fgmt.accesstoken a
                , cacheAuthFgmt (S.bookingsAuthToJson fgmt)
                ]
            )

        ( Nothing, Just fgmt ) ->
            ( { m
                | route = next
              }
            , Cmd.batch
                [ Nav.pushUrl m.navKey s.url
                , cacheAuthFgmt (S.bookingsAuthToJson fgmt)
                ]
            )

        _ ->
            ( { m
                | error = Message "Cannot route from state without token and url" :: m.error
                , bookingsAuth = Nothing
              }
            , Cmd.none
            )


switchToRoute : Model -> Url -> ( Model, Cmd Msg )
switchToRoute model url =
    let
        next =
            toRoute url
    in
    ( { model
        | route = next
      }
    , Nav.pushUrl model.navKey <| Url.toString url
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



-- Hosts --


fetchData : Model -> Cmd Msg
fetchData m =
    case ( m.route, m.bookingsAuth ) of
        ( Hosts, _ ) ->
            hostsReq

        ( Appointments hostId _, _ ) ->
            apptsReq hostId

        ( MyBookings _, Nothing ) ->
            setState (toJsonBookingState { appt = Nothing, url = "/bookings" })

        ( MyBookings _, Just fgmt ) ->
            bookingsGet fgmt.accesstoken

        _ ->
            Cmd.none


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


apptsUrl : HostId -> String
apptsUrl hostId =
    BuildUrl.absolute
        [ "api"
        , "hosts"
        , hostId
        , "appointments"
        ]
        []


apptsReq : HostId -> Cmd Msg
apptsReq hostId =
    Http.get
        { url = apptsUrl hostId
        , expect =
            Http.expectJson
                (GotAppointments hostId << Result.mapError HttpError)
                Host.readApointmentsResponse
        }


bookingsPost : String -> Appointment -> Cmd Msg
bookingsPost token appt =
    let
        auth =
            String.join " " [ "Bearer", token ]

        resp =
            BookingConfirm
                << Result.map (always appt)
                << Result.mapError HttpError
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" auth ]
        , url = apptsUrl appt.hostId
        , body = Http.jsonBody (Host.apptToJson appt)
        , expect = Http.expectWhatever resp
        , timeout = Nothing
        , tracker = Nothing
        }


bookingsGet : String -> Cmd Msg
bookingsGet token =
    let
        auth =
            String.join " " [ "Bearer", token ]
    in
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" auth ]
        , url = "https://local.byappt/api/bookings"
        , body = Http.emptyBody
        , expect = Http.expectJson (GotBookings << Result.mapError HttpError) Host.readApointmentsResponse
        , timeout = Nothing
        , tracker = Nothing
        }



-- decodeBookingsGet :


gotHostData : Model -> Result String Hosts -> ( Model, Cmd Msg )
gotHostData m res =
    case res of
        Err msg ->
            ( m, routeToError m msg )

        Ok hs ->
            ( { m | hosts = Just hs }, Cmd.none )


gotAppointmentsData : Model -> HostId -> Result Error (List Appointment) -> ( Model, Cmd Msg )
gotAppointmentsData m hostId res =
    case res of
        Err e ->
            ( { m | error = e :: m.error }, Cmd.none )

        Ok appts ->
            ( { m | route = Appointments hostId appts }, Cmd.none )


gotBookingsData : Model -> Result Error (List Appointment) -> ( Model, Cmd Msg )
gotBookingsData m res =
    case res of
        Err e ->
            ( { m | error = e :: m.error }, Cmd.none )

        Ok appts ->
            ( { m | route = MyBookings appts }, Cmd.none )


hostsTable : Maybe Hosts -> List (Html Msg)
hostsTable hs =
    [ Html.h2 [] [ Html.text "Hosts" ]
    , Html.table []
        [ Html.thead []
            [ Html.tr []
                [ Html.th [] [ Html.text "Name" ] ]
            ]
        , Html.tbody []
            (List.map
                (\( id, name ) ->
                    Html.tr []
                        [ Html.td []
                            [ Html.a
                                [ Attr.href (appointmentUrl id) ]
                                [ Html.text name ]
                            ]
                        ]
                )
                (Maybe.map Dict.toList hs
                    |> Maybe.withDefault []
                )
            )
        ]
    ]


appointmentsTable : Model -> List Appointment -> List (Html Msg)
appointmentsTable model appts =
    let
        book appt =
            case model.bookingsAuth of
                Just _ ->
                    Html.button [ onClick (Book appt) ] [ Html.text "Book" ]

                _ ->
                    Html.button [ Attr.disabled True ] [ Html.text "Book " ]
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
                        [ Html.td [] [ Html.text (String.fromInt x.start) ]
                        , Html.td [] [ Html.text (String.fromInt x.duration) ]
                        , Html.td [] [ book x ]
                        ]
                )
                appts
            )
        ]
    ]


hostsLink : Model -> Html Msg
hostsLink model =
    Html.a
        [ Attr.href <| BuildUrl.absolute [ "hosts" ] [] ]
        [ Html.text "Hosts"
        ]



-- Error


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



-- View


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
        [ Html.li [] [ hostsLink model ]
        , Html.li [] [ bookingsLink model ]
        ]


bookingsUrl : String
bookingsUrl =
    BuildUrl.absolute [ "bookings" ] []


bookingsLink : Model -> Html Msg
bookingsLink model =
    Html.a
        [ Attr.href bookingsUrl ]
        [ Html.text "Bookings" ]


content : Model -> List (Html Msg)
content model =
    case model.route of
        Hosts ->
            hostsTable model.hosts

        Appointments _ appts ->
            appointmentsTable model appts

        MyBookings bs ->
            [ Html.h2 [] [ Html.text "My bookings" ] ]

        Error msg ->
            errorPage msg

        _ ->
            [ Html.p [] [ Html.text "Not found" ] ]


appointmentUrl : HostId -> String
appointmentUrl hostId =
    BuildUrl.relative
        [ "hosts"
        , hostId
        , "appointments"
        ]
        []
