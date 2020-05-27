module Time.Helpers exposing
    ( monthToString
    , posixDecoder
    , posixEncode
    , posixToDate
    , posixToDateTime
    , posixToTime
    , weekdayToString
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Time exposing (Month(..), Posix, Weekday(..), Zone)


posixDecoder : Decoder Posix
posixDecoder =
    D.map (\int -> Time.millisToPosix (int * 1000)) D.int


posixEncode : Posix -> E.Value
posixEncode posix =
    E.int (Time.posixToMillis posix // 1000)


posixToDate : Zone -> Posix -> String
posixToDate zone posix =
    String.concat
        [ Time.toWeekday zone posix
            |> weekdayToString
        , " "
        , Time.toDay zone posix
            |> String.fromInt
            |> String.padLeft 2 '0'
        , "/"
        , Time.toMonth zone posix
            |> monthToInt
            |> String.fromInt
            |> String.padLeft 2 '0'
        , "/"
        , Time.toYear zone posix
            |> String.fromInt
            |> String.padLeft 4 '0'
        ]


posixToTime : Zone -> Posix -> String
posixToTime zone posix =
    String.concat
        [ Time.toHour zone posix
            |> String.fromInt
            |> String.padLeft 2 '0'
        , ":"
        , Time.toMinute zone posix
            |> String.fromInt
            |> String.padLeft 2 '0'
        ]


posixToDateTime : Zone -> Posix -> String
posixToDateTime zone posix =
    String.concat
        [ posixToDate zone posix
        , " "
        , posixToTime zone posix
        ]


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


monthToString : Month -> String
monthToString month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


weekdayToString : Weekday -> String
weekdayToString weekday =
    case weekday of
        Mon ->
            "Monday"

        Tue ->
            "Tuesday"

        Wed ->
            "Wednesday"

        Thu ->
            "Thursday"

        Fri ->
            "Friday"

        Sat ->
            "Saturday"

        Sun ->
            "Sunday"
