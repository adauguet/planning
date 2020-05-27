module FormatDate exposing (formatDate, monthString)

import Time exposing (Month(..), Posix, Weekday(..), Zone)


formatDate : Zone -> Posix -> String
formatDate zone posix =
    [ weekdayString zone posix
    , toDayString zone posix
    , monthString zone posix
    ]
        |> String.join " "


toDayString : Zone -> Posix -> String
toDayString zone =
    Time.toDay zone >> String.fromInt


weekdayString : Zone -> Posix -> String
weekdayString zone posix =
    case Time.toWeekday zone posix of
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
