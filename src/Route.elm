module Route exposing (Route(..), fromUrl, href, pushUrl, replaceUrl)

import Browser.Navigation as Nav exposing (Key)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes
import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)


type Route
    = Login
    | Home
    | NotFound


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Login (s "login")
        , map Home top
        ]


href : Route -> Attribute msg
href route =
    Html.Styled.Attributes.href (toString route)


pushUrl : Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (toString route)


replaceUrl : Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (toString route)


fromUrl : Url -> Route
fromUrl url =
    parse parser url |> Maybe.withDefault NotFound


toString : Route -> String
toString route =
    "/" ++ String.join "/" (toPieces route)


toPieces : Route -> List String
toPieces route =
    case route of
        Login ->
            [ "login" ]

        Home ->
            []

        NotFound ->
            [ "404" ]
