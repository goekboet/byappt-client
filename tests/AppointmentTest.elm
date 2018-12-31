module AppointmentTest exposing (suite)

import Expect exposing (..)
import Test exposing (..)

import Host exposing (..)
import Json.Decode as D
import Url as Url
import Url.Parser as Parse exposing (..)
import Main as Route exposing (Route(..))

toRoute s =
    case Url.fromString s of
        Just url ->
            Route.toRoute url

        _ ->
            NotFound

hostsResponseJson = "[{\"hostId\":\"HostId1\",\"friendlyName\":\"Adam\"},{\"hostId\":\"HostId2\",\"friendlyName\":\"Bertil\"},{\"hostId\":\"HostId3\",\"friendlyName\":\"Calle\"}]"

mockHosts : List Host
mockHosts =
    [ { id = "HostId1"
      , name = "Adam"
      }
    , { id = "HostId2"
      , name = "Bertil"
      }
    , { id = "HostId3"
      , name = "Calle"
      }
    ]

suite : Test
suite =
    describe "Appointment"
        [ test "map json to Host" <|
            \_ ->
                Expect.equal (D.decodeString readHostResponse hostsResponseJson)  (Ok mockHosts)
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
                    (Appointments "someHost")
                    (toRoute "https://local.byappt/hosts/someHost/appointments")
        , test "Can parse Bookings route" <|
            \_ ->
                Expect.equal
                    MyBookings
                    (toRoute "https://local.byappt/bookings")
        ]