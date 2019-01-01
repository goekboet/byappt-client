module AppointmentTest exposing (suite)

import Expect exposing (..)
import Test exposing (..)

import Host exposing (..)
import Json.Decode as D
import Dict
import Url as Url
import Url.Parser as Parse exposing (..)
import Main as Route exposing (Route(..))

toRoute s =
    case Url.fromString s of
        Just url ->
            Route.toRoute url

        _ ->
            NotFound

hostsResponseJson = "{\"HostId1\":\"Adam\",\"HostId2\":\"Bertil\",\"HostId3\":\"Calle\"}"

mockHosts =
    Dict.fromList
        [ ( "HostId1", "Adam" )
        , ( "HostId2", "Bertil" )
        , ( "HostId3", "Calle" )
        ]

appointmentsResponseJson = 
  """
  [{"hostId":"HostId1","start":1540972800,"duration":30},{"hostId":"HostId1","start":1540976400,"duration":30},{"hostId":"HostId1","start":1540980000,"duration":30},{"hostId":"HostId1","start":1540983600,"duration":30},{"hostId":"HostId1","start":1540990800,"duration":30},{"hostId":"HostId1","start":1540994400,"duration":30},{"hostId":"HostId1","start":1540998000,"duration":30}]
  """

mockAppointments : List Appointment
mockAppointments =
    [ Appointment "HostId1" 1540972800 30
    , Appointment "HostId1" 1540976400 30
    , Appointment "HostId1" 1540980000 30
    , Appointment "HostId1" 1540983600 30
    , Appointment "HostId1" 1540990800 30
    , Appointment "HostId1" 1540994400 30
    , Appointment "HostId1" 1540998000 30
    ]

suite : Test
suite =
    describe "Appointment"
        [ test "Parses Hosts from json" <|
            \_ ->
                Expect.equal (D.decodeString readHostResponse hostsResponseJson)  (Ok mockHosts)
        , test "Parses Appointments from json" <|
            \_ ->
                Expect.equal (D.decodeString readApointmentsResponse appointmentsResponseJson)  (Ok mockAppointments)
        , test "Maps /error to NotFound" <|
            \_ ->
                Expect.equal
                    NotFound
                    (toRoute "https://local.byappt/error")
        , test "Maps /error?msg=someMsg to Error" <|
            \_ ->
                Expect.equal
                    (Error "someMsg")
                    (toRoute "https://local.byappt/error?msg=someMsg")
        , test "Can parse Hosts route" <|
            \_ ->
                Expect.equal
                    Hosts
                    (toRoute "https://local.byappt/hosts")
        , test "Can parse Appointments route" <|
            \_ ->
                Expect.equal
                    (Appointments "someHost" [])
                    (toRoute "https://local.byappt/hosts/someHost/appointments")
        , test "Can parse Bookings route" <|
            \_ ->
                Expect.equal
                    MyBookings
                    (toRoute "https://local.byappt/bookings")
        ]