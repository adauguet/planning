port module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Code
import Css exposing (pct, px, rem)
import Css.Global
import Day exposing (Day)
import Day.Kind exposing (Kind)
import Derberos.Date.Calendar exposing (getCurrentMonthDates)
import Edit
import Helpers exposing (rangeStep)
import Html.Styled exposing (Attribute, Html, button, div, span, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Json.Encode as E exposing (Value)
import List.Extra
import Range exposing (Range)
import Tailwind
import Task
import Time exposing (Posix, Zone, millisToPosix, posixToMillis)
import Time.FR exposing (monthYearString, weekdayDayMonthString)
import Time.Helpers exposing (posixFromHoursMinutes)
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
    | ClickedValidate Int (Result String Kind)
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
            , Cmd.none
            )

        ClickedDay index day ->
            case ( model.zone, model.time ) of
                ( Just zone, Just time ) ->
                    ( { model | state = Edit index <| Edit.init zone time day.kind }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

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


encode : Zone -> Posix -> Model -> Value
encode zone posix model =
    E.object
        [ ( "name", E.string model.name )
        , ( "month", E.string <| monthYearString zone posix )
        , ( "days", E.list (Day.encodePDF zone) model.days )
        ]


port export : Value -> Cmd msg


computeDays : Zone -> Posix -> List Day
computeDays zone current =
    getCurrentMonthDates zone current
        |> List.map (\d -> { id = 0, date = d, kind = Day.Kind.Working [] })



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
            headers zone time

        _ ->
            text ""
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


from : Zone -> Posix -> Posix
from zone posix =
    posixFromHoursMinutes zone posix 7 30


to : Zone -> Posix -> Posix
to zone posix =
    posixFromHoursMinutes zone posix 19 0


step : Int
step =
    -- 15 minutes in millis
    15 * 60 * 1000


dayDuration : Zone -> Int
dayDuration zone =
    (to zone (millisToPosix 0) |> posixToMillis) - (from zone (millisToPosix 0) |> posixToMillis)


ticks : Zone -> Posix -> List Posix
ticks zone posix =
    rangeStep (from zone posix |> posixToMillis) (to zone posix |> posixToMillis) step
        |> List.map millisToPosix


slotView : Zone -> Int -> Posix -> Html msg
slotView zone units posix =
    let
        borderWidth =
            if Time.Helpers.isOnTheDot zone posix then
                2

            else
                1
    in
    div
        [ css
            [ Css.borderLeft3 (px borderWidth) Css.solid Tailwind.gray400
            , Css.lastChild [ Css.width Css.zero ]
            , Css.width (pct (100 / toFloat units))
            , Css.displayFlex
            ]
        ]
        []


hourView : Zone -> Int -> Posix -> Html msg
hourView zone units posix =
    let
        borderWidth =
            if Time.Helpers.isOnTheDot zone posix then
                2

            else
                1
    in
    div
        [ css
            [ Css.borderLeft3 (px borderWidth) Css.solid Tailwind.gray400
            , Css.lastChild [ Css.width Css.zero ]
            , Css.width (pct (100 / toFloat units))
            , Css.displayFlex
            , Css.position Css.relative
            ]
        ]
        [ if Time.Helpers.isOnTheDot zone posix then
            div
                [ css
                    [ Css.fontSize (px 10)
                    , Css.color Tailwind.gray500
                    , Css.fontWeight (Css.int 600)
                    , Css.position Css.absolute
                    , Css.left (px -14)
                    , Css.top (px -14)
                    ]
                ]
                [ text <| Time.Helpers.hourMinuteString zone posix ]

          else
            text ""
        ]


headers : Zone -> Posix -> Html Msg
headers zone posix =
    let
        t : List Posix
        t =
            ticks zone posix
    in
    div
        [ css
            [ Css.displayFlex
            , Css.height (px 10)
            ]
        ]
        [ div [ css [ Css.width (px 130), Css.padding (rem 1) ] ] []
        , div
            [ css
                [ Css.flexGrow (Css.int 1)
                , Css.displayFlex
                , Css.position Css.relative
                ]
            ]
            (List.map (hourView zone <| List.length t) t)
        , div
            [ css
                [ Css.width (px 156)
                , Css.padding (rem 0.5)
                ]
            ]
            []
        ]


dayView : Zone -> Posix -> ( Int, Day ) -> Html Msg
dayView zone today ( index, day ) =
    let
        t : List Posix
        t =
            ticks zone today
    in
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
                , Css.position Css.relative
                ]
            ]
            ((case day.kind of
                Day.Kind.Working ranges ->
                    List.map (rangeView zone today) ranges

                _ ->
                    []
             )
                ++ List.map (slotView zone <| List.length t) t
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
            [ if Day.workingHours day == 0 then
                text "-"

              else
                Day.workingHours day
                    |> Time.Helpers.formatDuration
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
            [ if Day.aapHours day == 0 then
                text "-"

              else
                Day.aapHours day
                    |> Time.Helpers.formatDuration
                    |> text
            ]
        ]


dateView : Zone -> Posix -> Posix -> Html msg
dateView zone today date =
    div
        [ css
            [ Css.width (px 130)
            , Css.padding (rem 1)
            , Css.color
                (if isSameDay zone today date then
                    Css.hex "4285F4"

                 else
                    Css.hex "000000"
                )
            ]
        ]
        [ text <| weekdayDayMonthString zone date
        ]


rangeView : Zone -> Posix -> Range -> Html msg
rangeView zone posix range =
    let
        left =
            (Time.Helpers.diff (from zone posix) range.begin |> Time.Helpers.millisToHours)
                / (dayDuration zone |> Time.Helpers.millisToHours)
                * 100

        width_ =
            (Range.duration range |> Time.Helpers.millisToHours)
                / (dayDuration zone |> Time.Helpers.millisToHours)
                * 100
    in
    div
        [ css
            [ Css.position Css.absolute
            , Css.top Css.zero
            , Css.left (pct left)
            , Css.width (pct width_)
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
                [ text <| Time.Helpers.hourMinuteString zone range.begin ++ " - " ++ Time.Helpers.hourMinuteString zone range.end ]
            ]
        ]


weekendView : Zone -> Posix -> Posix -> Html msg
weekendView zone today day =
    let
        t : List Posix
        t =
            ticks zone today
    in
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
                , Css.position Css.relative
                , Css.property "background-image" "linear-gradient(-45deg, transparent 49%, darkgray 49% 50%, transparent 50% 99%, darkgray 99%);"
                , Css.backgroundSize2 (px 10) (px 10)
                ]
            ]
            (List.map (slotView zone <| List.length t) t)
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
