module Endpoint exposing
    ( Endpoint(..)
    , day
    , days
    , daysFromTo
    , delete
    , forgotPassword
    , get
    , login
    , post
    , put
    )

import Http
import Time exposing (Posix)
import Url.Builder exposing (QueryParameter, crossOrigin, int)


type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


url : String -> List String -> List QueryParameter -> Endpoint
url host paths queryParams =
    crossOrigin host paths queryParams
        |> Endpoint



-- login


login : String -> Endpoint
login host =
    url host [ "login" ] []


forgotPassword : String -> Endpoint
forgotPassword host =
    url host [ "users", "forgot" ] []



-- days


days : String -> Endpoint
days host =
    url host [ "days" ] []


daysFromTo : String -> Posix -> Posix -> Endpoint
daysFromTo host from to =
    url host [ "days" ] [ int "from" <| Time.posixToMillis from // 1000, int "to" <| Time.posixToMillis to // 1000 ]


day : String -> Int -> Endpoint
day host id =
    url host [ "days", String.fromInt id ] []



-- helpers


get :
    { url : Endpoint
    , expect : Http.Expect a
    }
    -> Cmd a
get config =
    Http.riskyRequest
        { method = "get"
        , headers = []
        , url = unwrap config.url
        , body = Http.emptyBody
        , expect = config.expect
        , timeout = Nothing
        , tracker = Nothing
        }


post :
    { url : Endpoint
    , body : Http.Body
    , expect : Http.Expect a
    }
    -> Cmd a
post config =
    Http.riskyRequest
        { method = "post"
        , headers = []
        , url = unwrap config.url
        , body = config.body
        , expect = config.expect
        , timeout = Nothing
        , tracker = Nothing
        }


put :
    { url : Endpoint
    , body : Http.Body
    , expect : Http.Expect a
    }
    -> Cmd a
put config =
    Http.riskyRequest
        { method = "put"
        , headers = []
        , url = unwrap config.url
        , body = config.body
        , expect = config.expect
        , timeout = Nothing
        , tracker = Nothing
        }


delete :
    { url : Endpoint
    , expect : Http.Expect a
    }
    -> Cmd a
delete config =
    Http.riskyRequest
        { method = "delete"
        , headers = []
        , url = unwrap config.url
        , body = Http.emptyBody
        , expect = config.expect
        , timeout = Nothing
        , tracker = Nothing
        }
