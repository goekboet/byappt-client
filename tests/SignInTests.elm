module SignInTests exposing (suite)

import Expect exposing (..)
import SignIn exposing (..)
import Test exposing (..)
import Url as Url exposing (Protocol(..), Url)


suite : Test
suite =
    describe "My attempt at oidc implicit flow"
        [ test "Can make a valid url-string to auth endpoint" <|
            \_ -> Expect.equal expectedAuthUrl (SignIn.toAuthUrl someEndpoint someState)
        , test "Case1: Signinfragment with token" <|
            \_ -> Expect.equal case1Expect (parseFromString case1)
        , test "Case2: Error from auth-endpoint redirect" <|
            \_ -> Expect.equal case2Expect (parseFromString case2)
        , test "Case3: No fragment in url" <|
            \_ -> Expect.equal case3Expect (parseFromString case3)
        ]


someToken =
    "SomeToken"


someState =
    "SomeState"


someAuthRedirect =
    "SomeAuthRedirect"


someClientId =
    "SomeClientId"


someAuthEndpoint =
    "SomeAuthEndpoint"


someEndSessionEndpoint =
    "SomeEndSession"


someKeySetEndpoint =
    "SomeKeySetEndpoint"


someEndpoint : Endpoint
someEndpoint =
    { clientId = someClientId
    , authRedirect = someAuthRedirect
    , auth = someAuthEndpoint
    }


expectedAuthUrl : String
expectedAuthUrl =
    String.concat
        [ someAuthEndpoint
        , "/?client_id="
        , someClientId
        , "&redirect_uri="
        , someAuthRedirect
        , "&response_type=token"
        , "&scope=bookings"
        , "&state="
        , someState
        , "&nonce="
        , "n%2Fa"
        ]


expectedSignOutUrl : String
expectedSignOutUrl =
    String.concat
        [ someEndSessionEndpoint
        , "/?id_token_hint="
        , someToken
        , "&post_logout_redirect_uri="
        , someAuthRedirect
        ]


someError =
    "SomeError SomeDescription"


case1 =
    "https://local.byappt/#access_token=SomeToken&token_type=Bearer&expires_in=3600&scope=bookings&state=SomeState"


case1Expect =
    Just (Ok { accesstoken = someToken, expires = "3600", state = someState })


case2 =
    "https://local.byappt/#state=someState&error=SomeError&error_description=SomeDescription"


case2Expect =
    Just (Err someError)


case3 =
    "https://local.byappt"


case3Expect =
    Nothing


parseFromString : String -> Maybe (Result String BookingsAuth)
parseFromString s = 
    Url.fromString s
    |> Maybe.andThen .fragment
    |> Maybe.andThen parseBookingsAuth 