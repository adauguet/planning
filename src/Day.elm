module Day exposing (Day, Kind(..), aapHours, workingHours)

import Code
import Duration exposing (Duration)
import Range exposing (Range)
import Time exposing (Posix)


type alias Day =
    { date : Posix
    , kind : Kind
    }


type Kind
    = Default (List Range)
    | Holiday
    | Solidarity


workingHours : Day -> Duration
workingHours day =
    case day.kind of
        Default ranges ->
            ranges
                |> List.filter (.code >> Code.isPaid)
                |> Range.sum

        Holiday ->
            Duration.zero

        Solidarity ->
            Duration.zero


aapHours : Day -> Duration
aapHours day =
    case day.kind of
        Default ranges ->
            ranges
                |> List.filter (\r -> r.code == Code.AAP)
                |> Range.sum

        Holiday ->
            Duration.zero

        Solidarity ->
            Duration.zero
