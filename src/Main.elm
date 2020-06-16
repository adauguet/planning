port module Main exposing (main)

import Api
import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Code exposing (Code, backgroundColor)
import Css exposing (displayFlex, pct, px, rem)
import Css.Global
import Derberos.Date.Calendar exposing (getCurrentMonthDates, getFirstDayOfMonth, getLastDayOfMonth)
import Dict
import Edit
import Helpers exposing (rangeStep)
import Html.Styled exposing (Attribute, Html, button, div, span, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Http exposing (Error)
import Json.Encode as E exposing (Value)
import List.Extra
import Planning exposing (Planning)
import Posix
import Range exposing (Range)
import Tailwind
import Task
import Time exposing (Posix, Zone, millisToPosix, posixToMillis)
import Time.FR exposing (monthYearString, weekdayDayMonthString)
import Time.Helpers exposing (posixFromHoursMinutes)
import Url exposing (Url)



-- model


type alias Model =
    { host : String
    , zone : Maybe Zone
    , time : Maybe Posix
    , state : State
    , days : List Day
    , name : String
    }


type alias Day =
    { date : Posix
    , planning : Maybe Planning
    }


encodePDF : Zone -> Day -> Value
encodePDF zone day =
    let
        ranges =
            case day.planning of
                Just planning ->
                    planning.ranges

                Nothing ->
                    []
    in
    E.object
        [ ( "date", E.string <| weekdayDayMonthString zone day.date )
        , ( "is_weekend", E.bool <| Time.Helpers.isWeekend zone day.date )
        , ( "ranges", E.list (Range.encode <| Posix.encodePDF zone) ranges )
        ]


type State
    = Initial
    | Edit Day Edit.Model


init : String -> Url -> Key -> ( Model, Cmd Msg )
init host _ _ =
    ( { host = host
      , zone = Nothing
      , time = Nothing
      , state = Initial
      , days = []
      , name = "Antoine DAUGUET"
      }
    , Task.perform GotZoneTime <| Task.map2 Tuple.pair Time.here Time.now
    )



-- update


type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | GotZoneTime ( Zone, Posix )
    | ClickedDay Day
    | EditMsg Edit.Msg
    | ClickedCancel
    | ClickedValidate Day (Result String (List Range))
    | ClickedExport
    | GotPlannings (Result Error (List Planning))
    | DidPostDay (Result Error ())
    | DidPutPlanning (Result Error ())


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

        GotZoneTime ( here, now ) ->
            ( { model
                | time = Just now
                , zone = Just here
                , days = computeDays here now
              }
            , Api.getPlannings model.host (getFirstDayOfMonth here now) (getLastDayOfMonth here now) GotPlannings
            )

        ClickedDay day ->
            case ( model.zone, model.time ) of
                ( Just zone, Just time ) ->
                    case day.planning of
                        Just planning ->
                            ( { model | state = Edit day <| Edit.init zone time planning.ranges }, Cmd.none )

                        Nothing ->
                            ( { model | state = Edit day <| Edit.init zone time [] }, Cmd.none )

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

        ClickedValidate _ (Err _) ->
            ( model, Cmd.none )

        ClickedValidate day (Ok ranges) ->
            case day.planning of
                Just planning ->
                    ( { model | state = Initial }, Api.putPlanning model.host { planning | ranges = ranges } DidPutPlanning )

                Nothing ->
                    ( { model | state = Initial }, Api.postData model.host { date = day.date, ranges = ranges } DidPostDay )

        ClickedExport ->
            case ( model.zone, model.time ) of
                ( Just zone, Just time ) ->
                    ( model, export <| encode zone time model )

                _ ->
                    ( model, Cmd.none )

        GotPlannings (Ok plannings) ->
            ( { model | days = bar plannings model.days }, Cmd.none )

        GotPlannings (Err _) ->
            ( model, Cmd.none )

        DidPostDay (Ok ()) ->
            case ( model.zone, model.time ) of
                ( Just zone, Just time ) ->
                    ( model, Api.getPlannings model.host (getFirstDayOfMonth zone time) (getLastDayOfMonth zone time) GotPlannings )

                _ ->
                    ( model, Cmd.none )

        DidPostDay (Err _) ->
            ( model, Cmd.none )

        DidPutPlanning (Ok ()) ->
            case ( model.zone, model.time ) of
                ( Just zone, Just time ) ->
                    ( model, Api.getPlannings model.host (getFirstDayOfMonth zone time) (getLastDayOfMonth zone time) GotPlannings )

                _ ->
                    ( model, Cmd.none )

        DidPutPlanning (Err _) ->
            ( model, Cmd.none )


computeDays : Zone -> Posix -> List Day
computeDays zone current =
    getCurrentMonthDates zone current
        |> List.map (\posix -> { date = posix, planning = Nothing })


bar : List Planning -> List Day -> List Day
bar plannings days =
    let
        dict =
            plannings
                |> List.map (\planning -> ( Time.posixToMillis planning.date, planning ))
                |> Dict.fromList
    in
    List.map
        (\day ->
            case Dict.get (Time.posixToMillis day.date) dict of
                Just planning ->
                    { day | planning = Just planning }

                Nothing ->
                    day
        )
        days


encode : Zone -> Posix -> Model -> Value
encode zone posix model =
    E.object
        [ ( "name", E.string model.name )
        , ( "month", E.string <| monthYearString zone posix )
        , ( "days", E.list (encodePDF zone) model.days )
        ]


port export : Value -> Cmd msg



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
    , legend Code.selectList
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
                    |> List.map
                        (\day ->
                            if Time.Helpers.isWeekend zone day.date then
                                weekendView zone time day.date

                            else
                                dayView zone time day
                        )
                )

        _ ->
            text ""
    , case model.state of
        Initial ->
            text ""

        Edit day subModel ->
            modal [] [] [ Edit.view EditMsg ClickedCancel (ClickedValidate day) subModel ]
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


dayView : Zone -> Posix -> Day -> Html Msg
dayView zone today day =
    let
        t : List Posix
        t =
            ticks zone today
    in
    div
        [ onClick (ClickedDay day)
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
            ((case day.planning of
                Just planning ->
                    List.map (rangeView zone today) planning.ranges

                Nothing ->
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
            [ case day.planning of
                Just planning ->
                    let
                        sum =
                            Range.workingHours planning.ranges
                    in
                    if sum == 0 then
                        text "-"

                    else
                        sum
                            |> Time.Helpers.formatDuration
                            |> text

                Nothing ->
                    text "-"
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
            [ case day.planning of
                Just planning ->
                    let
                        sum =
                            Range.aapHours planning.ranges
                    in
                    if sum == 0 then
                        text "-"

                    else
                        sum
                            |> Time.Helpers.formatDuration
                            |> text

                Nothing ->
                    text "-"
            ]
        ]


dateView : Zone -> Posix -> Posix -> Html msg
dateView zone today date =
    div
        [ css
            [ Css.width (px 130)
            , Css.margin (rem 0.5)
            , Css.padding (rem 0.5)
            , Css.color
                (if isSameDay zone today date then
                    Css.hex "FFFFFF"

                 else
                    Css.hex "000000"
                )
            , Css.backgroundColor
                (if isSameDay zone today date then
                    Css.hex "4285F4"

                 else
                    Css.rgba 0 0 0 0
                )
            , Css.borderRadius (rem 0.2)
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


legend : List Code -> Html msg
legend codes =
    div
        [ css
            [ Css.padding (rem 1)
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.alignSelf Css.flexStart
            ]
        ]
        [ div [ css [ Css.padding (rem 0.4) ] ] [ text "Légende" ]
        , div
            [ css
                [ Css.displayFlex
                , Css.fontSize (px 12)
                , Css.overflow Css.auto
                , Css.flexDirection Css.column
                ]
            ]
            (List.map foo codes)
        ]


foo : Code -> Html msg
foo code =
    div
        [ css
            [ Css.margin (rem 0.1)
            , Css.borderRadius (rem 0.1)
            , Css.displayFlex
            , Css.alignItems Css.baseline
            ]
        ]
        [ div
            [ css
                [ Css.color <| Code.color code
                , Css.backgroundColor <| Code.backgroundColor code
                , Css.padding (rem 0.2)
                , Css.width (px 24)
                ]
            ]
            [ text <| Code.toString code ]
        , div [ css [ Css.marginLeft (rem 0.4) ] ] [ text <| Code.description code ]
        ]



-- main


main : Program String Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }
