module Day exposing (Day)

import Range exposing (Range)
import Time exposing (Posix)


type alias Day =
    { date : Posix
    , ranges : List Range
    }
