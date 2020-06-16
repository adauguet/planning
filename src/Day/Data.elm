module Day.Data exposing (Data, encode)

import Json.Encode as E exposing (Value)
import Posix
import Range exposing (Range)
import Time exposing (Posix)


type alias Data =
    { date : Posix
    , ranges : List Range
    }


encode : { a | date : Posix, ranges : List Range } -> Value
encode data =
    E.object
        [ ( "timestamp", Posix.encode data.date )
        , ( "ranges", E.list (Range.encode Posix.encode) data.ranges )
        ]
