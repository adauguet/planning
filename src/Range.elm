module Range exposing (Range, duration, toFloat)

import Code exposing (Code)


type alias Range =
    { begin : ( Int, Int )
    , end : ( Int, Int )
    , code : Code
    }


duration : Range -> Float
duration range =
    toFloat range.end - toFloat range.begin


toFloat : ( Int, Int ) -> Float
toFloat ( hours, minutes ) =
    Basics.toFloat hours + (Basics.toFloat minutes / 60)
