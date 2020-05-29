module Range exposing (Range, Time(..), duration, sum, toFloat, toMinutes)

import Code exposing (Code)
import Duration exposing (Duration)


type alias Range =
    { begin : Time
    , end : Time
    , code : Code
    }


duration : Range -> Duration
duration range =
    diff range.begin range.end


sum : List Range -> Duration
sum ranges =
    ranges
        |> List.map (duration >> Duration.toMinutes)
        |> List.foldr (+) 0
        |> Duration.fromMinutes


type Time
    = Time ( Int, Int )


toFloat : Time -> Float
toFloat (Time ( h, m )) =
    Basics.toFloat h + (Basics.toFloat m / 60)


toMinutes : Time -> Int
toMinutes (Time ( h, m )) =
    60 * h + m


diff : Time -> Time -> Duration
diff from to =
    toMinutes to - toMinutes from |> Duration.fromMinutes
