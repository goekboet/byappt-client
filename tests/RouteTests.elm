module RouteTests exposing (suite)

import Expect exposing (..)
import Test exposing (..)
import Route as Route exposing (Route(..))
import Url.Parser as Parse exposing (..)
import Url as Url

toRoute s =
    case Url.fromString s of
        Just url -> Route.toRoute url
        _ -> NotFound

suite : Test
suite =
    describe "Routing module"
        [ test "Can parse Home route" <|
            \_ -> Expect.equal 
                Home 
                (toRoute "https://local.byappt/")
        , test "Can parse Hosts route" <|
            \_ -> Expect.equal 
                Hosts 
                (toRoute "https://local.byappt/hosts")
        , test "Can parse Appointments route" <|
            \_ -> Expect.equal 
                (Appointments "someHost") 
                (toRoute "https://local.byappt/hosts/someHost/appointments")
        , test "Can parse Bookings route" <|
            \_ -> Expect.equal 
                MyBookings 
                (toRoute "https://local.byappt/bookings")
        ]
