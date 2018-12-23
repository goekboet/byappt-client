module Route exposing (..)

import Url.Parser as Parse exposing (Parser, (</>), string, parse)
import Url as Url exposing (Url)
import Url.Parser.Query as Query
import Appointment exposing (..)


type Route 
    = Hosts
    | Appointments HostId
    | MyBookings
    | NotFound
    | Error String

toRoute : Url -> Route
toRoute url =
    Maybe.withDefault NotFound (parse parser url)

parser : Parser (Route -> a) a
parser =
    let
        msg = Query.string "msg"
            |> Parse.query

        toError = Maybe.withDefault NotFound 
            << Maybe.map Error
    in
    Parse.oneOf
        [ Parse.map Hosts (Parse.s "hosts")
        , Parse.map Appointments (Parse.s "hosts" </> string </> Parse.s "appointments")
        , Parse.map MyBookings (Parse.s "bookings")
        , Parse.map toError (Parse.s "error" </> msg)
        ]


