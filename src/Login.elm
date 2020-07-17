module Login exposing (Model, Msg, init, subscriptions, update, view)

import Api
import Browser.Dom exposing (Element)
import Browser.Navigation exposing (Key)
import Element
    exposing
        ( Element
        , alignRight
        , column
        , el
        , fill
        , layout
        , none
        , paddingXY
        , paragraph
        , row
        , spacing
        , text
        , width
        )
import Element.Font as Font
import Element.Input as Input
import Hash
import Html exposing (Html)
import Http
import Json.Decode as D exposing (Error)
import Route exposing (pushUrl)
import Session exposing (Session(..))
import TailwindElmUI
import UI
import User exposing (User)


type alias Model =
    { key : Key
    , host : String
    , page : Page
    }


type Page
    = Login LoginModel
    | ForgotPassword String


type State
    = Initial
    | Error String


type alias LoginModel =
    { email : String
    , password : String
    , state : State
    }


init : Key -> String -> ( Model, Cmd Msg )
init key host =
    ( { key = key
      , host = host
      , page =
            Login
                { email = ""
                , password = ""
                , state = Initial
                }
      }
    , Cmd.none
    )



-- update


type Msg
    = InputEmail String
    | InputPassword String
    | ClickedLogin
    | HashedPassword String (Result D.Error String)
    | DidLogin (Result Http.Error User)
    | ClickedForgotPassword
    | InputResetEmail String
    | ClickedAskResetEmail
    | DidAskResetEmail (Result Http.Error ())
    | ClickedBack


update : Msg -> Model -> Session -> ( Model, Session, Cmd Msg )
update msg model session =
    case ( model.page, msg ) of
        ( Login m, InputEmail email ) ->
            ( { model | page = Login { m | email = email } }, session, Cmd.none )

        ( Login m, InputPassword password ) ->
            ( { model | page = Login { m | password = password } }, session, Cmd.none )

        ( Login m, ClickedLogin ) ->
            ( { model | page = Login { m | state = Initial } }, session, Hash.hash m.email m.password )

        ( Login _, HashedPassword email (Ok digest) ) ->
            ( model, session, Api.login model.host email digest DidLogin )

        ( Login m, HashedPassword _ (Err _) ) ->
            ( { model | page = Login { m | state = Error "A technical error occured." } }, session, Cmd.none )

        ( Login _, DidLogin (Ok user) ) ->
            ( model, LoggedIn user, pushUrl model.key Route.Home )

        ( Login m, DidLogin (Err _) ) ->
            ( { model | page = Login { m | state = Error "Invalid credentials." } }, session, Cmd.none )

        ( Login _, ClickedForgotPassword ) ->
            ( { model | page = ForgotPassword "" }, session, Cmd.none )

        ( Login _, _ ) ->
            ( model, session, Cmd.none )

        ( ForgotPassword _, ClickedBack ) ->
            ( { model | page = Login { email = "", password = "", state = Initial } }, session, Cmd.none )

        ( ForgotPassword _, InputResetEmail email ) ->
            ( { model | page = ForgotPassword email }, session, Cmd.none )

        ( ForgotPassword email, ClickedAskResetEmail ) ->
            ( model, session, Api.forgotPassword model.host email DidAskResetEmail )

        ( ForgotPassword _, _ ) ->
            ( model, session, Cmd.none )



-- view


view : Model -> Html Msg
view model =
    layout [ Font.size 14, Font.color TailwindElmUI.gray800 ]
        (case model.page of
            Login m ->
                loginView m

            ForgotPassword email ->
                forgotPasswordView email
        )


loginView : LoginModel -> Element Msg
loginView model =
    column
        UI.windowAttributes
        [ UI.h2 "Login"
        , paragraph [] [ text "Saisissez votre email et votre mot de passe." ]
        , case model.state of
            Initial ->
                none

            Error message ->
                el [ Font.color TailwindElmUI.red600, Font.size 13 ] <| text message
        , Input.email []
            { onChange = InputEmail
            , text = model.email
            , placeholder = Nothing
            , label = Input.labelAbove UI.inputLabelAttributes (text "Email")
            }
        , Input.currentPassword []
            { onChange = InputPassword
            , text = model.password
            , placeholder = Nothing
            , label = Input.labelAbove UI.inputLabelAttributes (text "Mot de passe")
            , show = False
            }
        , row [ width fill ]
            [ Input.button (paddingXY 0 12 :: UI.textButtonAttributes)
                { onPress = Just ClickedForgotPassword
                , label = text "Mot de passe oublié ?"
                }
            , Input.button (alignRight :: UI.plainButtonAttributes)
                { onPress = Just ClickedLogin
                , label = text "Connexion"
                }
            ]
        ]


forgotPasswordView : String -> Element Msg
forgotPasswordView email =
    column
        UI.windowAttributes
        [ Input.button UI.textButtonAttributes
            { onPress = Just ClickedBack
            , label = row [ spacing 4 ] [ UI.fontAwesomeIcon "fas fa-chevron-left", text "Retour" ]
            }
        , UI.h2 "Réinitialisation"
        , paragraph [] [ text "Merci de renseigner votre email pour réinitialiser votre mot de passe." ]
        , Input.email []
            { onChange = InputResetEmail
            , text = email
            , placeholder = Nothing
            , label = Input.labelAbove UI.inputLabelAttributes (text "Email")
            }
        , Input.button (alignRight :: UI.plainButtonAttributes)
            { onPress = Just ClickedAskResetEmail
            , label = text "Réinitialiser"
            }
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Login m ->
            Hash.hashed (D.decodeValue D.string >> HashedPassword m.email)

        ForgotPassword _ ->
            Sub.none
