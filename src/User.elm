module User exposing (User, decoder, fullName)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as DP
import Role exposing (Role)


type alias User =
    { email : String
    , firstName : String
    , lastName : String
    , role : Role
    }


decoder : Decoder User
decoder =
    D.succeed User
        |> DP.required "email" D.string
        |> DP.required "first_name" D.string
        |> DP.required "last_name" D.string
        |> DP.required "role" Role.decoder


fullName : User -> String
fullName user =
    user.firstName ++ " " ++ user.lastName
