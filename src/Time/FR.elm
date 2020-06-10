module Time.FR exposing (monthString, monthYearString, weekdayDayMonthString, weekdayString)

import Time exposing (Month(..), Posix, Weekday(..), Zone)


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


monthYearString : Zone -> Posix -> String
monthYearString zone posix =
    monthString zone posix ++ " " ++ (String.fromInt <| Time.toYear zone posix)


weekdayDayMonthString : Zone -> Posix -> String
weekdayDayMonthString zone posix =
    [ Time.toWeekday zone posix |> weekdayString
    , Time.toDay zone posix |> String.fromInt
    , monthString zone posix
    ]
        |> String.join " "
