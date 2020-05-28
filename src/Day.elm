module Day exposing (Day, Kind(..))

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
