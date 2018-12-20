module RouteTests exposing (suite)

import Expect exposing (..)
import Route as Route exposing (Route(..))
import Test exposing (..)
import Url as Url
import Url.Parser as Parse exposing (..)


toRoute s =
    case Url.fromString s of
        Just url ->
            Route.toRoute url

        _ ->
            NotFound


suite : Test
suite =
    describe "Routing module"
        [ test "Maps /error to NotFound" <|
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
