port module Hash exposing (hash, hashed)

import Json.Encode as E


port hash_ : E.Value -> Cmd msg


hash : String -> String -> Cmd msg
hash email password =
    E.object
        [ ( "password", E.string password )
        , ( "salt", E.string (email ++ "emeraude") )
        ]
        |> hash_


port hashed : (E.Value -> msg) -> Sub msg
