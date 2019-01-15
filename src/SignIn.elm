module SignIn exposing
    ( Endpoint
    , BookingsAuth
    , parseBookingsAuth
    , bookingsAuthFromJson
    , bookingsAuthToJson
    , toAuthUrl
    )

import Url.Builder as ToUrl
import Json.Decode as D
import Json.Encode as E

type alias Endpoint =
    { clientId : String
    , authRedirect : String
    , auth : String
    }


kvp : String -> String -> String
kvp k v =
    String.concat [ k, "=", v ]


queryString : List String -> String
queryString =
    String.concat << List.intersperse "&"


toAuthUrl : Endpoint -> String -> String
toAuthUrl ept state =
    let
        query =
            [ ToUrl.string "client_id" ept.clientId
            , ToUrl.string "redirect_uri" ept.authRedirect
            , ToUrl.string "response_type" "token"
            , ToUrl.string "scope" "bookings"
            , ToUrl.string "state" state
            , ToUrl.string "nonce" "n/a"
            ]
    in
    ToUrl.crossOrigin ept.auth [] query

parseQueryString : String -> List (List String)
parseQueryString =
    List.map (String.split "=") << String.split "&"


type alias BookingsAuth =
    { accesstoken : String
    , expires : String
    , state : String
    }

parseBookingsAuth : String -> Maybe (Result String BookingsAuth)
parseBookingsAuth f =
    case parseQueryString f of
        [ [ "access_token", a ]
          , [ "token_type", "Bearer" ]
          , [ "expires_in", b ]
          , [ "scope", "bookings" ]
          , [ "state", c ] ] ->
            Just (Ok <| BookingsAuth a b c)

        [ [ "state", _ ], [ "error", e ], [ "error_description", desc ] ] ->
            Just (Err <| String.join " " [ e, desc ])

        _ ->
            Nothing

bookingsAuthFromJson : D.Decoder BookingsAuth
bookingsAuthFromJson = D.map3 BookingsAuth
    (D.field "accesstoken" D.string)
    (D.field "expires" D.string)
    (D.field "state" D.string)

bookingsAuthToJson : BookingsAuth -> D.Value
bookingsAuthToJson b = E.object
    [ ("accesstoken", E.string b.accesstoken) 
    , ("expires", E.string b.expires)
    , ("state", E.string b.state)
    ]