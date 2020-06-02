module Time.Helpers exposing
    ( formatDate
    , isWeekend
    , monthString
    , monthYearString
    , posixDecoder
    , posixEncode
    , posixToDate
    , posixToDateTime
    , posixToTime
    , weekdayString
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
            |> weekdayString
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


isWeekend : Zone -> Posix -> Bool
isWeekend zone posix =
    case Time.toWeekday zone posix of
        Time.Sat ->
            True

        Time.Sun ->
            True

        _ ->
            False


monthYearString : Zone -> Posix -> String
monthYearString zone posix =
    monthString zone posix ++ " " ++ (String.fromInt <| Time.toYear zone posix)


formatDate : Zone -> Posix -> String
formatDate zone posix =
    [ Time.toWeekday zone posix |> weekdayString
    , toDayString zone posix
    , monthString zone posix
    ]
        |> String.join " "


toDayString : Zone -> Posix -> String
toDayString zone =
    Time.toDay zone >> String.fromInt


weekdayString : Weekday -> String
weekdayString weekday =
    case weekday of
        Mon ->
            "lundi"

        Tue ->
            "mardi"

        Wed ->
            "mercredi"

        Thu ->
            "jeudi"

        Fri ->
            "vendredi"

        Sat ->
            "samedi"

        Sun ->
            "dimanche"


monthString : Zone -> Posix -> String
monthString zone posix =
    case Time.toMonth zone posix of
        Jan ->
            "janvier"

        Feb ->
            "février"

        Mar ->
            "mars"

        Apr ->
            "avril"

        May ->
            "mai"

        Jun ->
            "juin"

        Jul ->
            "juillet"

        Aug ->
            "août"

        Sep ->
            "septembre"

        Oct ->
            "octobre"

        Nov ->
            "novembre"

        Dec ->
            "décembre"
