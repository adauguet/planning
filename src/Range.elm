module Range exposing (Range, decoder, duration, encode, sum)

import Code exposing (Code)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as DP
import Json.Encode as E exposing (Value)
import Posix
import Time exposing (Posix)


type alias Range =
    { begin : Posix
    , end : Posix
    , code : Code
    }


duration : Range -> Int
duration range =
    Time.posixToMillis range.end - Time.posixToMillis range.begin


sum : List Range -> Int
sum ranges =
    ranges
        |> List.map duration
        |> List.foldr (+) 0


encode : (Posix -> Value) -> Range -> Value
encode posixEncode range =
    E.object
        [ ( "begin", posixEncode range.begin )
        , ( "end", posixEncode range.end )
        , ( "code", E.string <| Code.toString range.code )
        ]


decoder : Decoder Range
decoder =
    D.succeed Range
        |> DP.required "begin" Posix.decoder
        |> DP.required "end" Posix.decoder
        |> DP.required "code" Code.decoder
