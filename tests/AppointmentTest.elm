module AppointmentTest exposing (suite)

import Expect exposing (..)
import Test exposing (..)


import Appointment exposing (..)
import Json.Decode as D

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
        ]