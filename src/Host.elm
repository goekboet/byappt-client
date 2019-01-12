module Host exposing
    ( Appointment
    , apptToJson
    , HostId
    , Hosts
    , readApointmentsResponse
    , readHostResponse
    )

import Dict exposing (..)
import Json.Decode as D
import Json.Encode as E
import Time exposing (..)


type alias HostId =
    String


type alias Name =
    String


type alias Hosts =
    Dict HostId Name


type alias Appointment =
    { hostId : HostId
    , start : Int
    , duration : Int
    }


readAppointment : D.Decoder Appointment
readAppointment =
    D.map3 Appointment
        (D.field "hostId" D.string)
        (D.field "start" D.int)
        (D.field "duration" D.int)

apptToJson : Appointment -> D.Value
apptToJson appt = E.object
    [ ("hostId", E.string appt.hostId)
    , ("start", E.int appt.start)
    , ("duration", E.int appt.duration)
    ]


readApointmentsResponse : D.Decoder (List Appointment)
readApointmentsResponse =
    D.list readAppointment


readHostResponse : D.Decoder Hosts
readHostResponse =
    D.dict D.string

millsSinceMonday : Zone -> Posix -> Int
millsSinceMonday z t =
    case toWeekday z t of
        Mon ->
            0

        Tue ->
            1

        Wed ->
            2

        Thu ->
            3

        Fri ->
            4

        Sat ->
            5

        Sun ->
            6


startOfMonday : Zone -> Posix -> Posix
startOfMonday z t =
    let
        hs =
            1000 * 60 * 60 * toHour z t

        m =
            1000 * 60 * toMinute z t

        s =
            1000 * toSecond z t

        ms =
            toMillis z t

        offset =
            86400000 * millsSinceMonday z t
    in
    millisToPosix <| posixToMillis t - (hs + m + s + ms + offset)
