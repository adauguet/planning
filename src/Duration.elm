module Duration exposing (Duration, description, fromMinutes, toFloat, toMinutes, zero)


type Duration
    = Duration ( Int, Int )


toMinutes : Duration -> Int
toMinutes (Duration ( h, m )) =
    60 * h + m


fromMinutes : Int -> Duration
fromMinutes m =
    Duration ( m // 60, modBy 60 m )


toFloat : Duration -> Float
toFloat (Duration ( h, m )) =
    Basics.toFloat h + (Basics.toFloat m / 60)


description : Duration -> String
description (Duration ( h, m )) =
    [ h, m ]
        |> List.map (String.fromInt >> String.padLeft 2 '0')
        |> String.join ":"


zero : Duration
zero =
    Duration ( 0, 0 )
