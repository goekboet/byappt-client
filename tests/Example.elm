module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Main
import Url exposing (Url, Protocol(..))

someRedirectUrl : Url
someRedirectUrl =
    { protocol = Url.Https
    , host = "somehost"
    , port_ = Nothing
    , path = ""
    , query = Nothing
    , fragment = Nothing
    }

someAuthEndpoint : Url
someAuthEndpoint =
    { protocol = Url.Https
    , host = "someAuthHost"
    , port_ = Nothing
    , path = ""
    , query = Nothing
    , fragment = Nothing
    }

someAuthRequest : Main.AuthRequest
someAuthRequest =
    { clientId = "someClientId"
    , redirectUri = someRedirectUrl
    , responseType = ["t1", "t2"]
    , scope = ["sc1", "sc2"]
    , state = "someState"
    , nonce = "someNonce"
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
falsePositive arg = (arg, isOk <| Main.parseSinginFragment arg)

expectedJwt : Main.Jwt
expectedJwt = 
    { header = "{\"alg\":\"HS256\",\"typ\":\"JWT\"}"
    , payload = "{\"sub\":\"1234567890\",\"name\":\"John Doe\",\"iat\":1516239022}"
    , signature = "SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"
    }

someUsername = "someUsername"
someNonce = "SomeNonce"
wellformedPayload = "{\"preferred_username\":\"someUsername\",\"nonce\":\"SomeNonce\"}"

suite : Test
suite =
    describe "My attempt at oidc implicit flow"
        [ test "Can make a valid url-string from authRequest" 
            <| \_ -> Expect.equal expectedAuthUrl (Url.toString <| Main.toAuthUrl someAuthEndpoint someAuthRequest)
        , test "Can parse signinfragment"
            <| \_ -> Expect.equal (Ok (mockToken, mockState)) (Main.parseSinginFragment wellFormed)
        , test "Errors on malformed signinfragment"
            <| \_ -> Expect.equal [] <| List.filter Tuple.second <| List.map falsePositive malFormed
        , test "Parses errormessage in signinFragment"
            <| \_ -> Expect.equal (Err someErrorMessage) <| Main.parseSinginFragment errorFragment
        , test "Decodes well-formed Jwt-token"
            <| \_ -> Expect.equal (Ok expectedJwt) (Main.jwtFromToken wellFormedToken)
        , test "Validates nonce and extracts username from payload"
            <| \_ -> Expect.equal (Ok someUsername) (Main.readUserName someNonce wellformedPayload)
        ]
