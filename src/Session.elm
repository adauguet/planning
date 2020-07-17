module Session exposing (Session(..))

import User exposing (User)


type Session
    = LoggedIn User
    | LoggedOut
