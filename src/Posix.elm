module Posix exposing (decoder, encode, encodePDF)

import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Time exposing (Posix, Zone)


decoder : Decoder Posix
decoder =
    D.map (\int -> Time.millisToPosix (int * 1000)) D.int


encode : Posix -> Value
encode posix =
    E.int (Time.posixToMillis posix // 1000)


encodePDF : Zone -> Posix -> Value
encodePDF zone posix =
    E.object
        [ ( "hours", E.int <| Time.toHour zone posix )
        , ( "minutes", E.int <| Time.toMinute zone posix )
        ]
