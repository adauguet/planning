module Time.Time exposing (Time(..), description, diff, encode, fromFloat, toFloat, toMinutes)

import Duration exposing (Duration)
import Json.Encode as E exposing (Value)


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


description : Time -> String
description (Time ( h, m )) =
    [ h, m ]
        |> List.map (String.fromInt >> String.padLeft 2 '0')
        |> String.join ":"


fromFloat : Float -> Time
fromFloat float =
    let
        minutes =
            float * 60 |> round
    in
    Time ( minutes // 60, modBy 60 minutes )


encode : Time -> Value
encode (Time ( h, m )) =
    E.object
        [ ( "hours", E.int h )
        , ( "minutes", E.int m )
        ]
