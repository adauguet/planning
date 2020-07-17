module Api exposing
    ( connect
    , forgotPassword
    , getPlannings
    , login
    , logout
    , postData
    , putPlanning
    )

import Day.Data exposing (Data)
import Endpoint
import Http exposing (Error)
import Json.Decode as D
import Json.Encode as E
import Planning exposing (Planning)
import Time exposing (Posix)
import User exposing (User)



-- login


login : String -> String -> String -> (Result Http.Error User -> msg) -> Cmd msg
login host email password msg =
    Endpoint.post
        { url = Endpoint.login host
        , body =
            Http.jsonBody
                (E.object
                    [ ( "email", E.string email )
                    , ( "password", E.string password )
                    ]
                )
        , expect = Http.expectJson msg User.decoder
        }


connect : String -> (Result Http.Error User -> msg) -> Cmd msg
connect host msg =
    Endpoint.get
        { url = Endpoint.login host
        , expect = Http.expectJson msg User.decoder
        }


forgotPassword : String -> String -> (Result Http.Error () -> msg) -> Cmd msg
forgotPassword host email msg =
    Endpoint.post
        { url = Endpoint.forgotPassword host
        , body = Http.jsonBody (E.object [ ( "email", E.string email ) ])
        , expect = Http.expectWhatever msg
        }


logout : String -> (Result Http.Error () -> msg) -> Cmd msg
logout host msg =
    Endpoint.delete
        { url = Endpoint.login host
        , expect = Http.expectWhatever msg
        }



--


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
