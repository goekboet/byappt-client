module Host exposing (..)

import Dict exposing (..)
import Time exposing (..)
import Json.Decode as D

type alias HostId = String

type alias Host =
    { id : HostId
    , name : String
    }

type alias Appointment =
    { start : String
    , duration : Int
    }

millsSinceMonday : Zone -> Posix -> Int
millsSinceMonday z t =
    case toWeekday z t of
        Mon -> 0
        Tue -> 1
        Wed -> 2
        Thu -> 3
        Fri -> 4
        Sat -> 5
        Sun -> 6


startOfMonday : Zone -> Posix -> Posix
startOfMonday z t =
    let 
        hs = 1000 * 60 * 60 * toHour z t
        m = 1000 * 60 * toMinute z t
        s = 1000 * toSecond z t
        ms = toMillis z t
        offset = 86400000 * millsSinceMonday z t
    in 
         millisToPosix <| (posixToMillis t) - (hs + m + s + ms + offset)

readHost : D.Decoder Host
readHost = D.map2 Host 
    (D.field "hostId" D.string)
    (D.field "friendlyName" D.string)

readHostResponse : D.Decoder (List Host)
readHostResponse = D.list readHost



mockAppointments : Dict HostId (List Appointment)
mockAppointments = fromList
    [ ("HostId1", [ Appointment "2018-12-24 08:00" 30
                   , Appointment "2018-12-24 09:00" 30
                   , Appointment "2018-12-24 10:00" 30
                   , Appointment "2018-12-24 11:00" 30
                   , Appointment "2018-12-24 13:00" 30
                   , Appointment "2018-12-24 14:00" 30
                   , Appointment "2018-12-24 15:00" 30
                   , Appointment "2018-12-24 16:00" 30
                   ])
    , ("HostId2", [ Appointment "2018-12-24 08:00" 30
                   , Appointment "2018-12-24 09:00" 30
                   , Appointment "2018-12-24 10:00" 30
                   , Appointment "2018-12-24 11:00" 30
                   , Appointment "2018-12-24 13:00" 30
                   , Appointment "2018-12-24 14:00" 30
                   , Appointment "2018-12-24 15:00" 30
                   , Appointment "2018-12-24 16:00" 30
                   ])
    , ("HostId3", [ Appointment "2018-12-24 08:00" 30
                   , Appointment "2018-12-24 09:00" 30
                   , Appointment "2018-12-24 10:00" 30
                   , Appointment "2018-12-24 11:00" 30
                   , Appointment "2018-12-24 13:00" 30
                   , Appointment "2018-12-24 14:00" 30
                   , Appointment "2018-12-24 15:00" 30
                   , Appointment "2018-12-24 16:00" 30
                   ])
    ]