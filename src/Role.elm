module Role exposing (Role(..), decoder, toString)

import Json.Decode as D exposing (Decoder)


type Role
    = Admin
    | User


decoder : Decoder Role
decoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "admin" ->
                        D.succeed Admin

                    "user" ->
                        D.succeed User

                    _ ->
                        D.fail ("could not decode role: " ++ s)
            )


toString : Role -> String
toString role =
    case role of
        Admin ->
            "Admin"

        User ->
            "User"
