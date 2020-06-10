module Day exposing (Data, Day, aapHours, decoder, encode, encodePDF, workingHours)

import Code
import Day.Kind exposing (Kind(..))
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as DP
import Json.Encode as E exposing (Value)
import Posix
import Range
import Time exposing (Posix, Zone)
import Time.FR exposing (weekdayDayMonthString)
import Time.Helpers


type alias Day =
    { id : Int
    , date : Posix
    , kind : Kind
    }


workingHours : Day -> Int
workingHours day =
    case day.kind of
        Working ranges ->
            ranges
                |> List.filter (.code >> Code.isPaid)
                |> Range.sum

        Holiday ->
            0

        Solidarity ->
            0


aapHours : Day -> Int
aapHours day =
    case day.kind of
        Working ranges ->
            ranges
                |> List.filter (\r -> r.code == Code.AAP)
                |> Range.sum

        Holiday ->
            0

        Solidarity ->
            0


encodePDF : Zone -> Day -> Value
encodePDF zone day =
    E.object
        ([ ( "date", E.string <| weekdayDayMonthString zone day.date )
         , ( "is_weekend", E.bool <| Time.Helpers.isWeekend zone day.date )
         ]
            ++ Day.Kind.encode (Posix.encodePDF zone) day.kind
        )


decoder : Decoder Day
decoder =
    D.succeed Day
        |> DP.required "id" D.int
        |> DP.required "date" Posix.decoder
        |> DP.required "kind" Day.Kind.decoder



-- data


type alias Data =
    { date : Posix
    , kind : Kind
    }


encode : Data -> Value
encode data =
    E.object
        (( "date", Posix.encode data.date )
            :: Day.Kind.encode Posix.encode data.kind
        )
