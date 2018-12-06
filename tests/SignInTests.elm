module SignInTests exposing (case1, case1Expect, case2, case2Expect, case3, case3Expect, expectedAuthUrl, expectedSignOutUrl, expectedSigninResponse, getValueOfMatchingKey, isOk, mockKeyDoc, parseFromString, rightKey, someAuthEndpoint, someAuthRedirect, someClientId, someEndSessionEndpoint, someEndpoint, someError, someKeySetEndpoint, someNonce, someOtherNonce, someSession, someState, someToken, suite, tokenWithNonce)

import Expect exposing (Expectation)
import SignIn exposing (..)
import Test exposing (..)
import Types exposing (..)
import Url exposing (Protocol(..), Url)


suite : Test
suite =
    describe "My attempt at oidc implicit flow"
        [ test "Can make a valid url-string to auth endpoint" <|
            \_ -> Expect.equal expectedAuthUrl (SignIn.toAuthUrl someEndpoint someSession)
        , test "Can make a valid url-string to signout endpoint" <|
            \_ -> Expect.equal expectedSignOutUrl (SignIn.toSignOutUrl someToken someEndpoint)
        , test "Case1: Signinfragment with token" <|
            \_ -> Expect.equal case1Expect (parseFromString case1)
        , test "Case2: Error from auth-endpoint redirect" <|
            \_ -> Expect.equal case2Expect (parseFromString case2)
        , test "Case3: No fragment in url" <|
            \_ -> Expect.equal case3Expect (parseFromString case3)
        , test "Validates nonce." <|
            \_ -> Expect.equal (Ok expectedSigninResponse) (SignIn.assertNonce someNonce tokenWithNonce)
        , test "Errors on nonce mismatch" <|
            \_ -> Expect.equal False (isOk <| SignIn.assertNonce someOtherNonce tokenWithNonce)
        , test "from json keydoc to value" <|
            \_ -> Expect.equal (Just (Ok "val1")) (getValueOfMatchingKey "kid1" mockKeyDoc)
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
    , endSession = someEndSessionEndpoint
    , keys = someKeySetEndpoint
    }


someSession : Session
someSession =
    { key = someState
    , nonce = someNonce
    }


expectedAuthUrl : String
expectedAuthUrl =
    String.concat
        [ someAuthEndpoint
        , "/?client_id="
        , someClientId
        , "&redirect_uri="
        , someAuthRedirect
        , "&response_type=id_token"
        , "&scope=openid"
        , "&state="
        , someState
        , "&nonce="
        , someNonce
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
    "https://local.byappt/#id_token=SomeToken&state=SomeState"


case1Expect =
    Just (Ok { idtoken = someToken, state = someState })


case2 =
    "https://local.byappt/#state=someState&error=SomeError&error_description=SomeDescription"


case2Expect =
    Just (Err someError)


case3 =
    "https://local.byappt"


case3Expect =
    Nothing


parseFromString : String -> Maybe (Result Error OidcAuthFragment)
parseFromString s =
    case Url.fromString s of
        Just url ->
            parseInitialUrl url

        _ ->
            Nothing


tokenWithNonce =
    "eyJraWQiOiJrZXBaZko4SFRoaFZ3MjREbWE2dG5uVmdHdlduWldnSXAxM1ZJZWlYelF3IiwiYWxnIjoiUlMyNTYifQ.eyJzdWIiOiIwMHVoZWt5cjg0MmlKZ3RZdzBoNyIsInZlciI6MSwiaXNzIjoiaHR0cHM6Ly9kZXYtOTg3ODA0Lm9rdGFwcmV2aWV3LmNvbS9vYXV0aDIvZGVmYXVsdCIsImF1ZCI6IjBvYWhkdjRnenBCWndQTTZTMGg3IiwiaWF0IjoxNTQyOTE0MDk3LCJleHAiOjE1NDI5MTc2OTcsImp0aSI6IklELkpkZjhONkYzMEtBYWlUVzJFclMwbUpfRWxBQ0hVdmJBSndmUFFxbUtJc3ciLCJhbXIiOlsicHdkIl0sImlkcCI6IjAwb2hla3BtemNTbHNGOVFEMGg3Iiwibm9uY2UiOiJzb21lTm9uY2UiLCJhdXRoX3RpbWUiOjE1NDI5MTM3Njd9.LfDZLZRXB4H4XwBZI0JS3Vv5bEffHP1alOInVSkgiGTH51o22qfCbdzuCavOEF5eANPMhTsJTV7Q9GSQ3h1gQDiuAdLqrevGSQV_8PrdJ1zDATP2Qno03WB5aSutKz9FObDc_H-CU1lzlvTP5Hcu5xjR6irR0J1zwKYE2DuBcO-8KjBXuZz3ebNzgNbFhTAsRqtWGAQOzxxEsLMPjoUMOykrz0cCuq5dVOzEuF50lmdmw3kesv9aIdu7xT3Sbx6CjMLN1NbyzTZsfZFvB1XJ7NqXEibybvJ6y6emJZyc61Gafu3acmlb4KVtlq-hyWXgJw441lvkq8f5luIicPO9Rg"


someNonce =
    "someNonce"


expectedSigninResponse =
    { kid = "kepZfJ8HThhVw24Dma6tnnVgGvWnZWgIp13VIeiXzQw"
    , jwt = tokenWithNonce
    }


someOtherNonce =
    "SomeOtherNonce"


isOk : Result a b -> Bool
isOk r =
    case r of
        Ok _ ->
            True

        Err _ ->
            False


getValueOfMatchingKey : Kid -> Jwks -> Maybe (Result Error String)
getValueOfMatchingKey kid keyset =
    getKey kid keyset
        |> Result.withDefault Nothing
        |> Maybe.map getValue


rightKey =
    "{ \"kid\": \"kid1\", \"otherKey\": \"val1\" }"


mockKeyDoc : Jwks
mockKeyDoc =
    """
    {
        "keys": [
            { "kid": "kid1", "n": "val1" },
            { "kid": "kid2", "n": "val2" }
        ]
    }
    """
