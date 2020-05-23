module Main exposing (main)

import Array exposing (Array)
import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Html.Styled exposing (Html, button, div, text, toUnstyled)
import Html.Styled.Events exposing (onClick)
import Range
import Url exposing (Url)



-- model


type alias Model =
    Array Range.Model


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( Array.fromList [ Range.init ]
    , Cmd.none
    )



-- update


type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | OnRangeMsg Int Range.Msg
    | ClickedAddRange


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "msg" msg
    in
    case msg of
        OnUrlChange _ ->
            ( model, Cmd.none )

        OnUrlRequest _ ->
            ( model, Cmd.none )

        OnRangeMsg index subMsg ->
            let
                subModel =
                    Array.get index model
            in
            case subModel of
                Just m ->
                    ( Array.set index (Range.update subMsg m) model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ClickedAddRange ->
            ( Array.push Range.init model, Cmd.none )



-- view


view : Model -> Document Msg
view model =
    { title = "Planning"
    , body =
        body model
            |> List.map toUnstyled
    }


body : Model -> List (Html Msg)
body model =
    [ div []
        (model
            |> Array.toList
            |> List.indexedMap rangeView
        )
    , button [ onClick ClickedAddRange ] [ text "Ajouter une plage" ]
    ]


rangeView : Int -> Range.Model -> Html Msg
rangeView index model =
    Range.view model
        |> Html.Styled.map (OnRangeMsg index)



-- main


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }
