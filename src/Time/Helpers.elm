module Time.Helpers exposing
    ( diff
    , formatDuration
    , hourMinuteString
    , isOnTheDot
    , isWeekend
    , millisToHours
    , posixFromHoursMinutes
    )

import Derberos.Date.Core exposing (civilToPosix, newDateRecord)
import Time exposing (Month(..), Posix, Weekday(..), Zone, posixToMillis)


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


posixFromHoursMinutes : Zone -> Posix -> Int -> Int -> Posix
posixFromHoursMinutes zone posix h min =
    let
        y =
            Time.toYear zone posix

        m =
            Time.toMonth zone posix |> monthToInt

        d =
            Time.toDay zone posix
    in
    newDateRecord y m d h min 0 0 zone |> civilToPosix


hourMinuteString : Zone -> Posix -> String
hourMinuteString zone posix =
    let
        h =
            Time.toHour zone posix
                |> String.fromInt
                |> String.padLeft 2 '0'

        m =
            Time.toMinute zone posix
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    h ++ ":" ++ m


isOnTheDot : Zone -> Posix -> Bool
isOnTheDot zone posix =
    Time.toMinute zone posix == 0


millisToHours : Int -> Float
millisToHours millis =
    let
        h =
            toFloat (millis // (60 * 60 * 1000))

        m =
            toFloat (remainderBy (60 * 60 * 1000) millis // (60 * 1000)) / 60
    in
    h + m


formatDuration : Int -> String
formatDuration millis =
    let
        h =
            millis
                // 3600000
                |> String.fromInt
                |> String.padLeft 2 '0'

        m =
            modBy 3600000 millis
                // (60 * 1000)
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    h ++ "h" ++ m


diff : Posix -> Posix -> Int
diff from to =
    posixToMillis to - posixToMillis from
