module Endpoint exposing
    ( Endpoint(..)
    , day
    , days
    , get
    , post
    , put
    )

import Http
import Url.Builder exposing (QueryParameter, crossOrigin)


type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


url : String -> List String -> List QueryParameter -> Endpoint
url host paths queryParams =
    crossOrigin host paths queryParams
        |> Endpoint



-- days


days : String -> Endpoint
days host =
    url host [ "days" ] []


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
