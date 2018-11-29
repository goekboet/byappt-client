module Discovery exposing (Endpoint, fromJson, getKey)

import Url exposing (Url)
import Json.Decode exposing (Decoder(..), field, string)
import Types exposing (..)

type alias Endpoint =
    { auth : String
    , endSession : String
    , keys : String
    }

fromJson : Decoder Endpoint
fromJson =
    let
        auth = field "authorization_endpoint" string
        signout = field "end_session_endpoint" string
        keys = field "jwks_uri" string
    in
        Json.Decode.map3 Endpoint auth signout keys

keysfield : Json.Decode.Decoder (List Json.Decode.Value)
keysfield = Json.Decode.field "keys" (Json.Decode.list Json.Decode.value)

matchKid : String -> Json.Decode.Value -> Bool
matchKid kid val =
    let
        kidfield = Json.Decode.field "kid" Json.Decode.string
    in
        Json.Decode.decodeValue kidfield val
            |> Result.map (\x -> x == kid)
            |> Result.withDefault False

getValue : Json.Decode.Value -> Result String String
getValue val =
    let
        valuefield = Json.Decode.field "n" Json.Decode.string
    in
        Json.Decode.decodeValue valuefield val
            |> Result.mapError (always "malformed value")

getKey : Kid -> Jwks -> Result String Jwk
getKey kid keyset = 
    Json.Decode.decodeString keysfield keyset
        |> Result.map (List.filter (matchKid kid))
        |> Result.mapError (always "malformed input")
        |> Result.map List.head
        |> Result.andThen (Result.fromMaybe "No key found")
