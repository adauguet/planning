module Range exposing (Range, duration, encode, sum)

import Code exposing (Code)
import Duration exposing (Duration)
import Json.Encode as E exposing (Value)
import Time.Time as Time exposing (Time)


type alias Range =
    { begin : Time
    , end : Time
    , code : Code
    }


duration : Range -> Duration
duration range =
    Time.diff range.begin range.end


sum : List Range -> Duration
sum ranges =
    ranges
        |> List.map (duration >> Duration.toMinutes)
        |> List.foldr (+) 0
        |> Duration.fromMinutes


encode : Range -> Value
encode range =
    E.object
        [ ( "begin", Time.encode range.begin )
        , ( "end", Time.encode range.end )
        , ( "code", E.string <| Code.toString range.code )
        ]
