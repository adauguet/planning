port module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Code
import Css exposing (pct, px, rem)
import Css.Global
import Day exposing (Day)
import Derberos.Date.Calendar exposing (getCurrentMonthDates)
import Duration exposing (Duration(..))
import Edit
import Html.Styled exposing (Attribute, Html, button, div, span, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Json.Encode as E exposing (Value)
import List.Extra
import Range exposing (Range)
import Tailwind
import Task
import Time exposing (Posix, Zone)
import Time.Helpers exposing (formatDate, monthYearString)
import Time.Time as Time
import Url exposing (Url)



-- model


type alias Model =
    { zone : Maybe Zone
    , time : Maybe Posix
    , state : State
    , days : List Day
    , name : String
    }


type State
    = Initial
    | Edit Int Edit.Model


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { zone = Nothing
      , time = Nothing
      , state = Initial
      , days = []
      , name = "Antoine DAUGUET"
      }
    , Task.perform GotZoneTime <| Task.map2 (\zone posix -> ( zone, posix )) Time.here Time.now
    )



-- update


type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | GotZoneTime ( Zone, Posix )
    | ClickedDay Int Day
    | EditMsg Edit.Msg
    | ClickedCancel
    | ClickedValidate Int (Result String Day.Kind)
    | ClickedExport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlChange _ ->
            ( model, Cmd.none )

        OnUrlRequest _ ->
            ( model, Cmd.none )

        GotZoneTime ( here, now ) ->
            ( { model
                | time = Just now
                , zone = Just here
                , days = computeDays here now
              }
                |> demo
            , Cmd.none
            )

        ClickedDay index day ->
            ( { model | state = Edit index <| Edit.init day.kind }, Cmd.none )

        EditMsg subMsg ->
            case model.state of
                Edit index subModel ->
                    let
                        ( m, cmd ) =
                            Edit.update subMsg subModel
                    in
                    ( { model | state = Edit index m }, Cmd.map EditMsg cmd )

                _ ->
                    ( model, Cmd.none )

        ClickedCancel ->
            ( { model | state = Initial }, Cmd.none )

        ClickedValidate index result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok kind ->
                    let
                        days =
                            case List.Extra.getAt index model.days of
                                Just day ->
                                    List.Extra.setAt index { day | kind = kind } model.days

                                Nothing ->
                                    model.days
                    in
                    ( { model | state = Initial, days = days }, Cmd.none )

        ClickedExport ->
            case ( model.zone, model.time ) of
                ( Just zone, Just time ) ->
                    ( model, export <| encode zone time model )

                _ ->
                    ( model, Cmd.none )


demo : Model -> Model
demo model =
    case List.Extra.getAt 2 model.days of
        Just day ->
            { model | days = List.Extra.setAt 2 { day | kind = Day.Default Edit.default } model.days }

        Nothing ->
            model


encode : Zone -> Posix -> Model -> Value
encode zone posix model =
    E.object
        [ ( "name", E.string model.name )
        , ( "month", E.string <| monthYearString zone posix )
        , ( "days", E.list (Day.encode zone) model.days )
        ]


port export : Value -> Cmd msg


computeDays : Zone -> Posix -> List Day
computeDays zone current =
    getCurrentMonthDates zone current
        |> List.map (\d -> { date = d, kind = Day.Default [] })



-- view


view : Model -> Document Msg
view model =
    { title = "Planning"
    , body = body model |> List.map toUnstyled
    }


body : Model -> List (Html Msg)
body model =
    [ Css.Global.global
        [ Css.Global.everything
            [ Css.fontFamilies [ "Roboto", .value Css.sansSerif ]
            , Css.focus [ Css.outline Css.zero ]
            ]
        , Css.Global.body
            [ Css.displayFlex
            , Css.flexDirection Css.column
            ]
        ]
    , div
        [ css
            [ Css.fontWeight (Css.int 600)
            , Css.fontSize (rem 1.5)
            , Css.padding (rem 1)
            ]
        ]
        [ case ( model.zone, model.time ) of
            ( Just zone, Just posix ) ->
                text <| model.name ++ " - " ++ monthYearString zone posix

            _ ->
                text model.name
        ]
    , div
        [ css [ Css.padding (rem 1), Css.alignSelf Css.flexEnd ] ]
        [ button [ onClick ClickedExport ] [ text "Exporter" ] ]
    , case ( model.zone, model.time ) of
        ( Just zone, Just time ) ->
            div []
                (model.days
                    |> List.indexedMap (\index day -> ( index, day ))
                    |> List.map
                        (\( index, day ) ->
                            if Time.Helpers.isWeekend zone day.date then
                                weekendView zone time day.date

                            else
                                dayView zone time ( index, day )
                        )
                )

        _ ->
            text ""
    , case model.state of
        Initial ->
            text ""

        Edit index subModel ->
            modal [] [] [ Edit.view EditMsg ClickedCancel (ClickedValidate index) subModel ]
    ]


from : Float
from =
    7.5


to : Float
to =
    19


dayView : Zone -> Posix -> ( Int, Day ) -> Html Msg
dayView zone today ( index, day ) =
    div
        [ onClick (ClickedDay index day)
        , css
            [ Css.displayFlex
            , Css.hover [ Css.backgroundColor Tailwind.gray200 ]
            , Css.height (px 50)
            ]
        ]
        [ dateView zone today day.date
        , div
            [ css
                [ Css.flexGrow (Css.int 1)
                , Css.displayFlex
                , Css.borderLeft3 (px 1) Css.solid Tailwind.gray400
                , Css.borderRight3 (px 1) Css.solid Tailwind.gray400
                , Css.position Css.relative
                ]
            ]
            ((case day.kind of
                Day.Default ranges ->
                    List.map rangeView ranges

                _ ->
                    []
             )
                ++ slotsView
            )
        , div
            [ css
                [ Css.width (px 70)
                , Css.padding (rem 0.5)
                , Css.displayFlex
                , Css.justifyContent Css.center
                , Css.alignItems Css.center
                ]
            ]
            [ if Day.workingHours day == Duration.zero then
                text "-"

              else
                Day.workingHours day
                    |> Duration.description
                    |> text
            ]
        , div
            [ css
                [ Css.width (px 70)
                , Css.padding (rem 0.5)
                , Css.displayFlex
                , Css.justifyContent Css.center
                , Css.alignItems Css.center
                ]
            ]
            [ if Day.aapHours day == Duration.zero then
                text "-"

              else
                Day.aapHours day
                    |> Duration.description
                    |> text
            ]
        ]


dateView : Zone -> Posix -> Posix -> Html msg
dateView zone today date =
    div
        [ css
            [ Css.width (px 130)
            , Css.padding (rem 1)
            , Css.displayFlex
            , Css.justifyContent Css.flexEnd
            , Css.alignItems Css.center
            ]
        ]
        [ div
            [ css
                [ Css.color
                    (if isSameDay zone today date then
                        Css.hex "4285F4"

                     else
                        Css.hex "000000"
                    )
                ]
            ]
            [ text <| formatDate zone date ]
        ]


units : Int
units =
    (to - from |> (*) 60 |> round) // 15


ticks : List Float
ticks =
    List.range (from * 4 |> round) (to * 4 |> round)
        |> List.map toFloat
        |> List.map (\e -> e / 4)
        |> dropLast


dropLast : List a -> List a
dropLast list =
    List.take (List.length list - 1) list


slotsView : List (Html msg)
slotsView =
    let
        firstCount =
            (from * 60 |> round |> modBy 60) // 15
    in
    (List.take firstCount ticks :: (List.drop firstCount ticks |> List.Extra.greedyGroupsOf 4))
        |> List.map slotView


slotView : List Float -> Html msg
slotView list =
    let
        size =
            List.length list
    in
    div
        [ css
            [ Css.borderRight3 (px 2) Css.solid Tailwind.gray400
            , Css.lastChild [ Css.border Css.zero ]
            , Css.width (pct (100 * toFloat size / toFloat units))
            , Css.displayFlex
            ]
        ]
        (List.repeat size
            (div
                [ css
                    [ Css.borderRight3 (px 1) Css.solid Tailwind.gray400
                    , Css.lastChild [ Css.border Css.zero ]
                    , Css.width (pct (100 / toFloat size))
                    ]
                ]
                []
            )
        )


rangeView : Range -> Html msg
rangeView range =
    div
        [ css
            [ Css.position Css.absolute
            , Css.top Css.zero
            , Css.left (pct <| (Time.toFloat range.begin - from) / (to - from) * 100)
            , Css.width (pct <| (Range.duration range |> Duration.toFloat) / (to - from) * 100)
            , Css.bottom Css.zero
            , Css.displayFlex
            ]
        ]
        [ div
            [ css
                [ Css.backgroundColor (Code.backgroundColor range.code)
                , Css.color (Code.color range.code)
                , Css.marginTop (rem 0.2)
                , Css.marginRight (rem 0.2)
                , Css.marginBottom (rem 0.2)
                , Css.flexGrow (Css.int 1)
                , Css.padding (rem 0.2)
                ]
            ]
            [ div [ css [ Css.fontSize (rem 0.9) ] ] [ text <| Code.toString range.code ]
            , span
                [ css
                    [ Css.fontSize (rem 0.7)
                    ]
                ]
                [ text <| Time.description range.begin ++ " - " ++ Time.description range.end ]
            ]
        ]


weekendView : Zone -> Posix -> Posix -> Html msg
weekendView zone today day =
    div
        [ css
            [ Css.displayFlex
            , Css.height (px 50)
            ]
        ]
        [ dateView zone today day
        , div
            [ css
                [ Css.flexGrow (Css.int 1)
                , Css.displayFlex
                , Css.borderLeft3 (px 1) Css.solid Tailwind.gray400
                , Css.borderRight3 (px 1) Css.solid Tailwind.gray400
                , Css.position Css.relative
                , Css.property "background-image" "linear-gradient(-45deg, transparent 49%, darkgray 49% 50%, transparent 50% 99%, darkgray 99%);"
                , Css.backgroundSize2 (px 10) (px 10)
                ]
            ]
            slotsView
        , div
            [ css
                [ Css.width (px 156)
                , Css.padding (rem 0.5)
                ]
            ]
            []
        ]


width : Float
width =
    800


isSameDay : Zone -> Posix -> Posix -> Bool
isSameDay zone posix1 posix2 =
    (Time.toYear zone posix1 == Time.toYear zone posix2)
        && (Time.toMonth zone posix1 == Time.toMonth zone posix2)
        && (Time.toDay zone posix1 == Time.toDay zone posix2)


modal : List (Attribute msg) -> List (Attribute msg) -> List (Html msg) -> Html msg
modal modalAttributes attributes elements =
    div
        (css
            [ Css.display Css.block
            , Css.position Css.fixed
            , Css.zIndex (Css.int 1)
            , Css.left Css.zero
            , Css.top Css.zero
            , Css.width (pct 100)
            , Css.height (pct 100)
            , Css.overflow Css.auto
            , Css.backgroundColor (Css.rgba 0 0 0 0.4)
            , Css.displayFlex
            , Css.justifyContent Css.center
            , Css.alignItems Css.center
            ]
            :: modalAttributes
        )
        [ div
            (css
                [ Css.backgroundColor Tailwind.white
                , Css.padding (rem 2)
                , Tailwind.rounded
                , Css.displayFlex
                , Css.flexDirection Css.column
                ]
                :: attributes
            )
            elements
        ]



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
