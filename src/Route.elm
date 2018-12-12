module Route exposing (toRoute, Route(..))

import Url.Parser as Parse exposing (Parser, (</>), string, parse)
import Url as Url exposing (Url)

type alias HostId = String

type Route 
    = Home
    | Hosts
    | Appointments HostId
    | MyBookings
    | NotFound

toRoute : Url -> Route
toRoute url =
    Maybe.withDefault NotFound (parse parser url)

parser : Parser (Route -> a) a
parser =
    Parse.oneOf
        [ Parse.map Home Parse.top
        , Parse.map Hosts (Parse.s "hosts")
        , Parse.map Appointments (Parse.s "hosts" </> string </> Parse.s "appointments")
        , Parse.map MyBookings (Parse.s "bookings")
        ]

