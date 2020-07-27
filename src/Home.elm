port module Home exposing (Model, Msg, init, update, view)

import Api
import Browser.Navigation exposing (Key)
import Code exposing (Code)
import Day exposing (Day)
import Derberos.Date.Calendar exposing (getCurrentMonthDates, getFirstDayOfMonth, getLastDayOfMonth)
import Dict
import Edit
import Element
    exposing
        ( Attribute
        , Element
        , centerX
        , centerY
        , column
        , el
        , fill
        , fillPortion
        , height
        , inFront
        , layout
        , minimum
        , mouseOver
        , moveRight
        , moveUp
        , none
        , padding
        , paddingEach
        , paddingXY
        , px
        , rgb255
        , rgba
        , rgba255
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Helpers exposing (rangeStep)
import Html exposing (Html)
import Http exposing (Error)
import Json.Encode as E exposing (Value)
import Planning exposing (Planning)
import Range exposing (Range)
import Route
import Session exposing (Session)
import Tailwind
import Task
import Time exposing (Posix, Zone, millisToPosix, posixToMillis, toWeekday)
import Time.FR exposing (monthYearString, weekdayDayMonthString)
import Time.Helpers exposing (diff, isSameDay, isWeekend, posixFromHoursMinutes)
import User exposing (User)


type alias Model =
    { key : Key
    , host : String
    , zone : Maybe Zone
    , time : Maybe Posix
    , state : State
    , days : List Day
    , user : User
    }


type State
    = Initial
    | Edit Day Edit.Model


init : Key -> String -> User -> ( Model, Cmd Msg )
init key host user =
    ( { key = key
      , host = host
      , zone = Nothing
      , time = Nothing
      , state = Initial
      , days = []
      , user = user
      }
    , Task.perform GotZoneTime <| Task.map2 Tuple.pair Time.here Time.now
    )



-- update


type Msg
    = GotZoneTime ( Zone, Posix )
    | ClickedDay Day
    | EditMsg Edit.Msg
    | ClickedCancel
    | ClickedValidate Day (Result String (List Range))
    | ClickedExport
    | GotPlannings (Result Error (List Planning))
    | DidPostDay (Result Error ())
    | DidPutPlanning (Result Error ())
    | ClickedLogout
    | DidLogout (Result Error ())


update : Msg -> Model -> Session -> ( Model, Session, Cmd Msg )
update msg model session =
    case msg of
        GotZoneTime ( here, now ) ->
            ( { model
                | time = Just now
                , zone = Just here
                , days = computeDays here now
              }
            , session
            , Api.getPlannings model.host (getFirstDayOfMonth here now) (getLastDayOfMonth here now) GotPlannings
            )

        ClickedDay day ->
            case ( model.zone, model.time ) of
                ( Just zone, Just time ) ->
                    case toWeekday zone day.date of
                        Time.Sun ->
                            ( model, session, Cmd.none )

                        _ ->
                            case day.planning of
                                Just planning ->
                                    ( { model | state = Edit day <| Edit.init zone time planning.ranges }, session, Cmd.none )

                                Nothing ->
                                    ( { model | state = Edit day <| Edit.init zone time [] }, session, Cmd.none )

                _ ->
                    ( model, session, Cmd.none )

        EditMsg subMsg ->
            case model.state of
                Edit index subModel ->
                    let
                        ( m, cmd ) =
                            Edit.update subMsg subModel
                    in
                    ( { model | state = Edit index m }, session, Cmd.map EditMsg cmd )

                _ ->
                    ( model, session, Cmd.none )

        ClickedCancel ->
            ( { model | state = Initial }, session, Cmd.none )

        ClickedValidate _ (Err _) ->
            ( model, session, Cmd.none )

        ClickedValidate day (Ok ranges) ->
            case day.planning of
                Just planning ->
                    ( { model | state = Initial }, session, Api.putPlanning model.host { planning | ranges = ranges } DidPutPlanning )

                Nothing ->
                    ( { model | state = Initial }, session, Api.postData model.host { date = day.date, ranges = ranges } DidPostDay )

        ClickedExport ->
            case ( model.zone, model.time ) of
                ( Just zone, Just time ) ->
                    ( model, session, export <| encode zone time model )

                _ ->
                    ( model, session, Cmd.none )

        GotPlannings (Ok plannings) ->
            -- let
            --     _ =
            --         Debug.log "plannings" <| applyPlannings plannings model.days
            -- in
            ( { model | days = applyPlannings plannings model.days }, session, Cmd.none )

        GotPlannings (Err _) ->
            ( model, session, Cmd.none )

        DidPostDay (Ok ()) ->
            case ( model.zone, model.time ) of
                ( Just zone, Just time ) ->
                    ( model, session, Api.getPlannings model.host (getFirstDayOfMonth zone time) (getLastDayOfMonth zone time) GotPlannings )

                _ ->
                    ( model, session, Cmd.none )

        DidPostDay (Err _) ->
            ( model, session, Cmd.none )

        DidPutPlanning (Ok ()) ->
            case ( model.zone, model.time ) of
                ( Just zone, Just time ) ->
                    ( model, session, Api.getPlannings model.host (getFirstDayOfMonth zone time) (getLastDayOfMonth zone time) GotPlannings )

                _ ->
                    ( model, session, Cmd.none )

        DidPutPlanning (Err _) ->
            ( model, session, Cmd.none )

        ClickedLogout ->
            ( model, session, Api.logout model.host DidLogout )

        DidLogout (Ok _) ->
            ( model, Session.LoggedOut, Route.pushUrl model.key Route.Home )

        DidLogout (Err _) ->
            ( model, session, Cmd.none )


computeDays : Zone -> Posix -> List Day
computeDays zone current =
    getCurrentMonthDates zone current
        |> List.map (\posix -> { date = posix, planning = Nothing })


applyPlannings : List Planning -> List Day -> List Day
applyPlannings plannings days =
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
        [ ( "name", E.string <| User.fullName model.user )
        , ( "month", E.string <| monthYearString zone posix )
        , ( "days", E.list (Day.encodePDF zone) model.days )
        ]


port export : Value -> Cmd msg



-- view


view : Model -> Html Msg
view model =
    layout
        [ width fill
        , Font.family [ Font.typeface "Roboto", Font.sansSerif ]
        , case model.state of
            Initial ->
                inFront none

            Edit day subModel ->
                inFront <|
                    el [ Background.color <| rgba255 0 0 0 0.4, height fill, width fill ] <|
                        el [ centerX, centerY, Background.color Tailwind.white, padding 16 ] <|
                            Edit.view EditMsg ClickedCancel (ClickedValidate day) subModel
        ]
        (column [ width fill ]
            [ case ( model.zone, model.time ) of
                ( Just zone, Just posix ) ->
                    text <| User.fullName model.user ++ " - " ++ monthYearString zone posix

                _ ->
                    text <| User.fullName model.user
            , Input.button
                [ paddingXY 10 5
                , Border.rounded 3
                , Border.width 1
                , Font.size 13
                ]
                { onPress = Just ClickedLogout, label = text "Déconnexion" }
            , Input.button
                [ paddingXY 10 5
                , Border.rounded 3
                , Border.width 1
                , Font.size 13
                ]
                { onPress = Just ClickedExport, label = text "Exporter" }
            , legend Code.selectList
            , case ( model.zone, model.time ) of
                ( Just here, Just today ) ->
                    let
                        dayViews =
                            List.map (dayView here today) model.days
                    in
                    el
                        [ width (fill |> minimum 500)
                        ]
                    <|
                        column [ width fill ] (headers here today :: dayViews)

                _ ->
                    none
            ]
        )


from : Zone -> Posix -> Posix
from zone posix =
    posixFromHoursMinutes zone posix 7 30


to : Zone -> Posix -> Posix
to zone posix =
    posixFromHoursMinutes zone posix 19 0


ticks : Zone -> Posix -> List Posix
ticks zone posix =
    rangeStep (from zone posix |> posixToMillis) (to zone posix |> posixToMillis) (15 * 60 * 1000)
        |> List.map millisToPosix


legend : List Code -> Element msg
legend codes =
    column
        [ padding 8, spacing 8 ]
        [ text "Légende"
        , column [ spacing 4 ] <| List.map codeView codes
        ]


codeView : Code -> Element msg
codeView code =
    row [ spacing 4 ]
        [ el
            [ Font.color <| Code.colorE code
            , Background.color <| Code.backgroundColorE code
            , padding 2
            , width <| px 24
            , height fill
            ]
            none
        , el [ Font.size 12, padding 4 ] <| text <| Code.description code
        ]


headers : Zone -> Posix -> Element msg
headers zone posix =
    let
        t : List Posix
        t =
            ticks zone posix
                |> (\l -> List.take (List.length l - 1) l)
    in
    row [ width fill, height <| px 5 ]
        [ el [ width <| px 182 ] none
        , row [ width fill, height fill ] <| List.map (tickView zone) t
        ]


tickView : Zone -> Posix -> Element msg
tickView zone posix =
    let
        borderWidth =
            if Time.Helpers.isOnTheDot zone posix then
                2

            else
                1

        tickLabel =
            if Time.Helpers.isOnTheDot zone posix then
                el
                    [ Font.size 10
                    , Font.color Tailwind.gray500
                    , Font.heavy
                    , moveRight -14
                    , moveUp 13
                    ]
                <|
                    text <|
                        Time.Helpers.hourMinuteString zone posix

            else
                none
    in
    el
        [ width fill
        , height fill
        , Border.widthEach { top = 0, right = 0, bottom = 0, left = borderWidth }
        , Border.color Tailwind.gray400
        , padding 5
        , inFront <| tickLabel
        ]
        none


dayView : Zone -> Posix -> Day -> Element Msg
dayView zone today day =
    let
        t : List Posix
        t =
            ticks zone today
                |> (\l -> List.take (List.length l - 1) l)

        planningView =
            row
                ([ width fill
                 , height fill
                 ]
                    ++ rangesView zone today day
                )
            <|
                List.map (slotView zone) t

        backgroundColor =
            if isWeekend zone day.date then
                Tailwind.gray200

            else
                Tailwind.white
    in
    row
        [ width fill
        , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
        , Border.color Tailwind.gray400
        , mouseOver [ Background.color Tailwind.gray200 ]
        , onClick <| ClickedDay day
        , Background.color backgroundColor
        ]
        [ dateView zone today day.date
        , planningView
        ]


rangesView : Zone -> Posix -> Day -> List (Attribute msg)
rangesView zone posix day =
    case day.planning of
        Just planning ->
            planning.ranges
                |> List.map (rangeView zone posix)
                |> List.map inFront

        Nothing ->
            []


dateView : Zone -> Posix -> Posix -> Element msg
dateView zone today date =
    let
        textColor =
            if isSameDay zone today date then
                rgb255 255 255 255

            else
                rgb255 0 0 0

        backgroundColor =
            if isSameDay zone today date then
                rgb255 66 133 244

            else
                rgba 0 0 0 0
    in
    weekdayDayMonthString zone date
        |> text
        |> el
            [ padding 8
            , Background.color backgroundColor
            , Font.color textColor
            , Font.size 14
            , Border.rounded 3
            ]
        |> el
            [ width <| px 182
            , padding 8
            ]


slotView : Zone -> Posix -> Element msg
slotView zone posix =
    let
        borderWidth =
            if Time.Helpers.isOnTheDot zone posix then
                2

            else
                1
    in
    el
        [ width fill
        , height fill
        , Border.widthEach { top = 0, right = 0, bottom = 0, left = borderWidth }
        , Border.color Tailwind.gray400
        , padding 5
        ]
        none


rangeView : Zone -> Posix -> Range -> Element msg
rangeView zone posix range =
    row [ width fill, height fill ]
        [ el [ width <| fillPortion <| diff (from zone posix) range.begin ] none
        , el
            [ paddingEach { top = 3, right = 4, bottom = 3, left = 0 }
            , width <| fillPortion <| Range.duration range
            , height fill
            ]
            (column
                [ paddingXY 5 3
                , Background.color <| Code.backgroundColorE range.code
                , Font.color <| Code.colorE range.code
                , width fill
                , height fill
                , spacing 2
                ]
                [ el [ Font.size 15 ] <| text <| Code.toString range.code
                , el [ Font.size 11 ] <| text <| Time.Helpers.hourMinuteString zone range.begin ++ " - " ++ Time.Helpers.hourMinuteString zone range.end
                ]
            )
        , el
            [ width <| fillPortion <| diff range.end (to zone posix) ]
            none
        ]
