module Main exposing (main)

import Api
import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Home
import Html
import Http exposing (Error)
import Login
import Route exposing (Route)
import Session exposing (Session(..))
import Url exposing (Url)
import User exposing (User)



-- model


type alias Model =
    { key : Key
    , host : String
    , page : Page
    , session : Session
    }


type Page
    = Login Login.Model
    | Home Home.Model
    | Redirect


init : String -> Url -> Key -> ( Model, Cmd Msg )
init host url key =
    ( { key = key
      , host = host
      , page = Redirect
      , session = LoggedOut
      }
    , Api.connect host <| DidConnect <| Route.fromUrl url
    )



-- update


type Msg
    = DidConnect Route (Result Error User)
    | GotLoginMsg Login.Msg
    | GotHomeMsg Home.Msg
    | UrlChanged Url
    | UrlRequested UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.page, msg ) of
        ( _, DidConnect route (Ok user) ) ->
            changeRouteTo route { model | session = LoggedIn user }

        ( _, DidConnect route (Err _) ) ->
            changeRouteTo route model

        ( _, UrlChanged url ) ->
            changeRouteTo (Route.fromUrl url) model

        ( _, UrlRequested urlRequest ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Route.pushUrl model.key (Route.fromUrl url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        ( Home subModel, GotHomeMsg subMsg ) ->
            Home.update subMsg subModel model.session
                |> updateWithSession model Home GotHomeMsg

        ( Login subModel, GotLoginMsg subMsg ) ->
            Login.update subMsg subModel model.session
                |> updateWithSession model Login GotLoginMsg

        _ ->
            ( model, Cmd.none )


changeRouteTo : Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    case model.session of
        LoggedIn user ->
            case route of
                Route.Login ->
                    ( { model | page = Redirect }, Route.pushUrl model.key Route.Home )

                Route.Home ->
                    Home.init model.key model.host user
                        |> updateWith model Home GotHomeMsg

                Route.NotFound ->
                    Home.init model.key model.host user
                        |> updateWith model Home GotHomeMsg

        LoggedOut ->
            case route of
                Route.Login ->
                    Login.init model.key model.host
                        |> updateWith model Login GotLoginMsg

                _ ->
                    ( { model | session = LoggedOut, page = Redirect }, Route.pushUrl model.key Route.Login )


updateWith : Model -> (page -> Page) -> (msg -> Msg) -> ( page, Cmd msg ) -> ( Model, Cmd Msg )
updateWith model toPage toMsg ( subModel, subCmd ) =
    ( { model | page = toPage subModel }, Cmd.map toMsg subCmd )


updateWithSession : Model -> (page -> Page) -> (msg -> Msg) -> ( page, Session, Cmd msg ) -> ( Model, Cmd Msg )
updateWithSession model toPage toMsg ( subModel, session, subCmd ) =
    ( { model | page = toPage subModel, session = session }, Cmd.map toMsg subCmd )



-- view


view : Model -> Document Msg
view model =
    case model.page of
        Redirect ->
            { title = "..."
            , body = []
            }

        Login subModel ->
            { title = "Login"
            , body =
                Login.view subModel
                    |> Html.map GotLoginMsg
                    |> List.singleton
            }

        Home subModel ->
            { title = "Home"
            , body = [ Home.view subModel |> Html.map GotHomeMsg ]
            }



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Login subModel ->
            Login.subscriptions subModel
                |> Sub.map GotLoginMsg

        _ ->
            Sub.none



-- main


main : Program String Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }
