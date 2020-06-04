module Demo exposing (ranges)

import Code exposing (Code(..))
import Range exposing (Range)
import Time.Time exposing (Time(..))


ranges : List Range
ranges =
    [ { begin = Time ( 7, 30 )
      , end = Time ( 8, 30 )
      , code = Code.T
      }
    , { begin = Time ( 8, 30 )
      , end = Time ( 10, 0 )
      , code = Code.TT
      }
    , { begin = Time ( 10, 0 )
      , end = Time ( 11, 30 )
      , code = Code.HS
      }
    , { begin = Time ( 11, 30 )
      , end = Time ( 13, 0 )
      , code = Code.NT
      }
    , { begin = Time ( 13, 0 )
      , end = Time ( 14, 30 )
      , code = Code.RCR
      }
    , { begin = Time ( 14, 30 )
      , end = Time ( 16, 0 )
      , code = Code.AGE
      }
    , { begin = Time ( 16, 0 )
      , end = Time ( 17, 30 )
      , code = Code.AT
      }
    , { begin = Time ( 17, 30 )
      , end = Time ( 19, 0 )
      , code = Code.AAP
      }
    ]
