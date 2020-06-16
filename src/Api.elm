module Api exposing (getPlannings, postData, putPlanning)

import Day.Data exposing (Data)
import Endpoint
import Http exposing (Error)
import Json.Decode as D
import Planning exposing (Planning)
import Time exposing (Posix)


getPlannings : String -> Posix -> Posix -> (Result Error (List Planning) -> msg) -> Cmd msg
getPlannings host from to msg =
    Endpoint.get
        { url = Endpoint.daysFromTo host from to
        , expect = Http.expectJson msg (D.list Planning.decoder)
        }


postData : String -> Data -> (Result Error () -> msg) -> Cmd msg
postData host data msg =
    Endpoint.post
        { url = Endpoint.days host
        , body = Http.jsonBody (Day.Data.encode data)
        , expect = Http.expectWhatever msg
        }


putPlanning : String -> Planning -> (Result Error () -> msg) -> Cmd msg
putPlanning host planning msg =
    Endpoint.put
        { url = Endpoint.day host planning.id
        , body = Http.jsonBody (Day.Data.encode planning)
        , expect = Http.expectWhatever msg
        }
