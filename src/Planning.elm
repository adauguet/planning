module Planning exposing (Planning, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as DP
import Posix
import Range exposing (Range)
import Time exposing (Posix)


type alias Planning =
    { id : Int
    , date : Posix
    , ranges : List Range
    }


decoder : Decoder Planning
decoder =
    D.succeed Planning
        |> DP.required "id" D.int
        |> DP.required "timestamp" Posix.decoder
        |> DP.required "ranges" (D.list Range.decoder)
