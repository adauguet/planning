module Api exposing (getDays, postDay, putDay)

import Day exposing (Day)
import Endpoint
import Http exposing (Error)
import Json.Decode as D


getDays : String -> (Result Error (List Day) -> msg) -> Cmd msg
getDays host msg =
    Endpoint.get
        { url = Endpoint.days host
        , expect = Http.expectJson msg (D.list Day.decoder)
        }


postDay : String -> Day.Data -> (Result Error () -> msg) -> Cmd msg
postDay host data msg =
    Endpoint.post
        { url = Endpoint.days host
        , body = Http.jsonBody (Day.encode data)
        , expect = Http.expectWhatever msg
        }


putDay : String -> Int -> Day.Data -> (Result Error () -> msg) -> Cmd msg
putDay host id data msg =
    Endpoint.put
        { url = Endpoint.day host id
        , body = Http.jsonBody (Day.encode data)
        , expect = Http.expectWhatever msg
        }
