module Day exposing (Day, encodePDF)

import Json.Encode as E exposing (Value)
import Planning exposing (Planning)
import Posix
import Range
import Time exposing (Posix, Zone)
import Time.FR exposing (weekdayDayMonthString)
import Time.Helpers


type alias Day =
    { date : Posix
    , planning : Maybe Planning
    }


encodePDF : Zone -> Day -> Value
encodePDF zone day =
    let
        ranges =
            case day.planning of
                Just planning ->
                    planning.ranges

                Nothing ->
                    []
    in
    E.object
        [ ( "date", E.string <| weekdayDayMonthString zone day.date )
        , ( "is_weekend", E.bool <| Time.Helpers.isWeekend zone day.date )
        , ( "ranges", E.list (Range.encode <| Posix.encodePDF zone) ranges )
        ]
