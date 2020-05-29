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
    String.concat
        [ h
            |> String.fromInt
            |> String.padLeft 2 '0'
        , ":"
        , m
            |> String.fromInt
            |> String.padLeft 2 '0'
        ]


zero : Duration
zero =
    Duration ( 0, 0 )
