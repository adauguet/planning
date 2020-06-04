module Day exposing (Day, Kind(..), aapHours, encode, workingHours)

import Code
import Duration exposing (Duration)
import Json.Encode as E exposing (Value)
import Range exposing (Range)
import Time exposing (Posix, Zone)
import Time.Helpers


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


encode : Zone -> Day -> Value
encode zone day =
    E.object
        ([ ( "date", E.string <| Time.Helpers.formatDate zone day.date )
         , ( "is_weekend", E.bool <| Time.Helpers.isWeekend zone day.date )
         ]
            ++ encodeKind day.kind
        )


encodeKind : Kind -> List ( String, Value )
encodeKind kind =
    case kind of
        Default ranges ->
            [ ( "kind", E.string "default" )
            , ( "ranges", E.list Range.encode ranges )
            ]

        Holiday ->
            [ ( "kind", E.string "holiday" ) ]

        Solidarity ->
            [ ( "kind", E.string "solidarity" ) ]
