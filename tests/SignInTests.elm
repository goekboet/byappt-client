module SignInTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Url exposing (Url, Protocol(..))
import Types exposing (..)
import SignIn 

someRedirectUrl : Url
someRedirectUrl =
    { protocol = Url.Https
    , host = "somehost"
    , port_ = Nothing
    , path = ""
    , query = Nothing
    , fragment = Nothing
    }

someEndpoint : Url
someEndpoint =
    { protocol = Url.Https
    , host = "someAuthHost"
    , port_ = Nothing
    , path = ""
    , query = Nothing
    , fragment = Nothing
    }

someAuthRequest : AuthRequest
someAuthRequest =
    { clientId = "someClientId"
    , redirectUri = someRedirectUrl
    , responseType = ["t1", "t2"]
    , scope = ["sc1", "sc2"]
    , state = "someState"
    , nonce = "someNonce"
    }

someSignOutRequest : SignOut
someSignOutRequest =
    { redirect = someRedirectUrl
    , token = mockToken
    }

expectedAuthUrl : String
expectedAuthUrl = 
    String.concat
    [ "https://someAuthHost"
    , "?client_id=someClientId"
    , "&redirect_uri=https://somehost"
    , "&response_type=t1 t2"
    , "&scope=sc1 sc2"
    , "&state=someState"
    , "&nonce=someNonce"
    ]

expectedSignOutUrl : String
expectedSignOutUrl = 
    String.concat
    [ "https://someAuthHost"
    , "?id_token_hint=someToken"
    , "&post_logout_redirect_uri=https://somehost"
    ]



wellFormedToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"
mockToken = "someToken"
mockState = "someState"
wellFormed = "id_token=someToken&state=someState"
malFormed = [ "id_token=someTokenstate=someState"
            , "id_tokensomeToken&state=someState"
            , "id_token=someToken&statesomeState"
            , "=someToken&state=someState"
            , "id_token=someToken&=someState"
            , "_=someToken&state=someState"
            ]
someErrorMessage = "someErrorMessage"
errorFragment = "state=someState&error=someErrorMessage"

isOk : Result a b -> Bool
isOk r = case r of
    Ok _ -> True
    Err _ -> False

falsePositive : String -> (String, Bool)     
falsePositive arg = (arg, isOk <| SignIn.parseFragment arg)

someUsername = "someUsername"
someNonce = "someNonce"
someOtherNonce = "someOtherNonce"
tokenWithNonce = "eyJraWQiOiJrZXBaZko4SFRoaFZ3MjREbWE2dG5uVmdHdlduWldnSXAxM1ZJZWlYelF3IiwiYWxnIjoiUlMyNTYifQ.eyJzdWIiOiIwMHVoZWt5cjg0MmlKZ3RZdzBoNyIsInZlciI6MSwiaXNzIjoiaHR0cHM6Ly9kZXYtOTg3ODA0Lm9rdGFwcmV2aWV3LmNvbS9vYXV0aDIvZGVmYXVsdCIsImF1ZCI6IjBvYWhkdjRnenBCWndQTTZTMGg3IiwiaWF0IjoxNTQyOTE0MDk3LCJleHAiOjE1NDI5MTc2OTcsImp0aSI6IklELkpkZjhONkYzMEtBYWlUVzJFclMwbUpfRWxBQ0hVdmJBSndmUFFxbUtJc3ciLCJhbXIiOlsicHdkIl0sImlkcCI6IjAwb2hla3BtemNTbHNGOVFEMGg3Iiwibm9uY2UiOiJzb21lTm9uY2UiLCJhdXRoX3RpbWUiOjE1NDI5MTM3Njd9.LfDZLZRXB4H4XwBZI0JS3Vv5bEffHP1alOInVSkgiGTH51o22qfCbdzuCavOEF5eANPMhTsJTV7Q9GSQ3h1gQDiuAdLqrevGSQV_8PrdJ1zDATP2Qno03WB5aSutKz9FObDc_H-CU1lzlvTP5Hcu5xjR6irR0J1zwKYE2DuBcO-8KjBXuZz3ebNzgNbFhTAsRqtWGAQOzxxEsLMPjoUMOykrz0cCuq5dVOzEuF50lmdmw3kesv9aIdu7xT3Sbx6CjMLN1NbyzTZsfZFvB1XJ7NqXEibybvJ6y6emJZyc61Gafu3acmlb4KVtlq-hyWXgJw441lvkq8f5luIicPO9Rg"

suite : Test
suite =
    describe "My attempt at oidc implicit flow"
        [ test "Can make a valid url-string to auth endpoint" 
            <| \_ -> Expect.equal expectedAuthUrl (Url.toString <| SignIn.toAuthUrl someEndpoint someAuthRequest)
        , test "Can make a valid url-string to signout endpoint"
            <| \_ -> Expect.equal expectedSignOutUrl (Url.toString <| SignIn.toSignOutUrl someEndpoint someSignOutRequest)
        , test "Can parse signinfragment"
            <| \_ -> Expect.equal (Ok (mockToken, mockState)) (SignIn.parseFragment wellFormed)
        , test "Errors on malformed signinfragment"
            <| \_ -> Expect.equal [] <| List.filter Tuple.second <| List.map falsePositive malFormed
        , test "Parses errormessage in signinFragment"
            <| \_ -> Expect.equal (Err someErrorMessage) <| SignIn.parseFragment errorFragment
        -- , test "Validates nonce."
        --     <| \_ -> Expect.equal (Ok tokenWithNonce) (SignIn.assertNonce someNonce tokenWithNonce)
        , test "Errors on nonce mismatch"
            <| \_ -> Expect.equal False (isOk <| SignIn.assertNonce someOtherNonce tokenWithNonce)
        ]
