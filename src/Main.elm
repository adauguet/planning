module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Code
import Css exposing (pct, px, rem, zero)
import Date
import Day exposing (Day)
import Derberos.Date.Calendar exposing (getCurrentMonthDates)
import Duration exposing (Duration(..))
import Edit
import FormatDate exposing (formatDate, monthString)
import Html.Styled exposing (Attribute, Html, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css, id)
import Html.Styled.Events exposing (onClick)
import List.Extra
import Range exposing (Range)
import Svg.Attributes
import Svg.Styled exposing (Svg)
import Svg.Styled.Attributes
import Tailwind
import Task
import Time exposing (Posix, Zone)
import Url exposing (Url)



-- model


type alias Model =
    { zone : Maybe Zone
    , time : Maybe Posix
    , state : State
    , days : List Day
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


daysToWeeks : Zone -> List Day -> List ( Int, List ( Int, Day ) )
daysToWeeks zone days =
    let
        weekNumber =
            Date.fromPosix zone >> Date.weekNumber
    in
    days
        |> List.indexedMap (\index day -> ( index, day ))
        |> List.Extra.groupWhile (\a b -> weekNumber (Tuple.second a).date == weekNumber (Tuple.second b).date)
        |> List.map (\( head, tail ) -> ( weekNumber (Tuple.second head).date, head :: tail ))


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
    [ div
        [ css
            [ Css.fontWeight (Css.int 600)
            , Css.fontSize (rem 1.5)
            , Css.padding (rem 1)
            ]
        ]
        [ text "Antoine DAUGUET" ]
    , case ( model.zone, model.time ) of
        ( Just zone, Just time ) ->
            let
                weeks =
                    daysToWeeks zone model.days
            in
            div [] (monthHeader zone time :: headers :: List.map (weekView zone time) weeks)

        _ ->
            text ""
    , case model.state of
        Initial ->
            text ""

        Edit index subModel ->
            modal [] [] [ Edit.view EditMsg ClickedCancel (ClickedValidate index) subModel ]
    ]


headers : Html msg
headers =
    let
        h =
            10
    in
    div [ css [ Css.displayFlex ] ]
        [ div
            [ css
                [ Css.padding2 Css.zero (rem 1)
                , Css.width (Css.px 110)
                ]
            ]
            []
        , div
            [ css
                [ Css.width (Css.px 120)
                , Css.displayFlex
                , Css.alignItems Css.center
                , Css.justifyContent Css.flexEnd
                , Css.padding2 Css.zero (rem 0.5)
                ]
            ]
            []
        , div [ css [ Css.marginLeft (rem 1) ] ]
            [ Svg.Styled.svg
                [ Svg.Attributes.width width
                , Svg.Attributes.height h
                , Svg.Attributes.viewBox 0 0 width h
                ]
                (List.range (ceiling from) (floor to)
                    |> List.map toFloat
                    |> List.map (\e -> e - from)
                    |> List.map ((*) (width / numberOfHours))
                    |> List.map
                        (\x ->
                            Svg.Styled.line
                                [ Svg.Attributes.x1 x
                                , Svg.Attributes.y1 0
                                , Svg.Attributes.x2 x
                                , Svg.Attributes.y2 h
                                , Svg.Styled.Attributes.stroke "darkgray"
                                ]
                                []
                        )
                )
            ]
        ]


weekView : Zone -> Posix -> ( Int, List ( Int, Day ) ) -> Html Msg
weekView zone currentDate ( n, days ) =
    div [ css [ Css.displayFlex ] ]
        [ div
            [ css
                [ Css.padding2 Css.zero (rem 1)
                , Css.width (Css.px 110)
                ]
            ]
            [ text <| "Semaine " ++ String.fromInt n ]
        , div [ css [ Css.padding2 Css.zero (rem 1) ] ] (List.map (dayView zone currentDate) days)
        ]


monthHeader : Zone -> Posix -> Html msg
monthHeader zone posix =
    div
        [ css
            [ Css.fontSize (rem 1.5)
            , Css.fontWeight (Css.int 900)
            , Css.padding (rem 1)
            ]
        ]
        [ text <| monthString zone posix ++ " " ++ (String.fromInt <| Time.toYear zone posix)
        ]


dayView : Zone -> Posix -> ( Int, Day ) -> Html Msg
dayView zone today ( index, day ) =
    div
        [ css
            [ Css.displayFlex
            , Css.cursor Css.default
            , if isSameDay zone day.date today then
                Css.batch
                    [ Css.color Tailwind.blue700
                    , Css.fontWeight (Css.int 500)
                    ]

              else
                Css.hover
                    [ Css.backgroundColor Tailwind.gray200
                    ]
            ]
        , onClick (ClickedDay index day)
        ]
        [ div
            [ css
                [ Css.minWidth (Css.px 120)
                , Css.displayFlex
                , Css.alignItems Css.center
                , Css.justifyContent Css.flexEnd
                ]
            ]
            [ div [] [ text <| formatDate zone day.date ] ]
        , div [ css [ Css.marginLeft (rem 1) ] ]
            [ case day.kind of
                Day.Default ranges ->
                    if isWeekend zone day.date then
                        weekendView

                    else
                        planningView ranges

                Day.Holiday ->
                    weekendView

                Day.Solidarity ->
                    weekendView
            ]
        , div
            [ css
                [ Css.displayFlex
                , Css.alignItems Css.center
                , Css.margin2 Css.zero (rem 1)
                ]
            ]
            [ div []
                [ Day.workingHours day
                    |> Duration.description
                    |> text
                ]
            ]
        , div
            [ css
                [ Css.displayFlex
                , Css.alignItems Css.center
                , Css.margin2 Css.zero (rem 1)
                ]
            ]
            [ div []
                [ Day.aapHours day
                    |> Duration.description
                    |> text
                ]
            ]
        ]


isWeekend : Zone -> Posix -> Bool
isWeekend zone posix =
    case Time.toWeekday zone posix of
        Time.Sat ->
            True

        Time.Sun ->
            True

        _ ->
            False


width : Float
width =
    800


dayHeight : Float
dayHeight =
    40


from : Float
from =
    7.5


to : Float
to =
    19


numberOfHours : Float
numberOfHours =
    to - from


planningView : List Range -> Html msg
planningView ranges =
    Svg.Styled.svg
        [ Svg.Attributes.width width
        , Svg.Attributes.height dayHeight
        , Svg.Attributes.viewBox 0 0 width dayHeight
        ]
        (frame
            :: ticks dayHeight
            ++ (List.map rangeSvg ranges |> List.concat)
        )


weekendView : Html msg
weekendView =
    Svg.Styled.svg
        [ Svg.Attributes.width width
        , Svg.Attributes.height dayHeight
        , Svg.Attributes.viewBox 0 0 width dayHeight
        ]
        (frame
            :: Svg.Styled.pattern
                [ Svg.Styled.Attributes.id "stripes"
                , Svg.Attributes.x 0
                , Svg.Attributes.y 0
                , Svg.Attributes.width 10
                , Svg.Attributes.height 10
                , Svg.Styled.Attributes.patternUnits "userSpaceOnUse"
                ]
                [ Svg.Styled.line
                    [ Svg.Attributes.x1 10
                    , Svg.Attributes.y1 0
                    , Svg.Attributes.x2 0
                    , Svg.Attributes.y2 10
                    , Svg.Styled.Attributes.stroke "darkgray"
                    ]
                    []
                ]
            :: Svg.Styled.rect
                [ Svg.Attributes.x 0
                , Svg.Attributes.y 0
                , Svg.Attributes.width width
                , Svg.Attributes.height dayHeight
                , Svg.Styled.Attributes.fill "url(#stripes)"
                ]
                []
            :: ticks dayHeight
        )


frame : Svg msg
frame =
    Svg.Styled.rect
        [ Svg.Attributes.x 0
        , Svg.Attributes.y 0
        , Svg.Attributes.width width
        , Svg.Attributes.height dayHeight
        , Svg.Styled.Attributes.fill "#F7FAFC"
        ]
        []


rangeSvg : Range -> List (Svg msg)
rangeSvg range =
    let
        x =
            range.begin
                |> Range.toFloat
                |> (\i -> i - from)
                |> (*) (width / numberOfHours)
    in
    [ Svg.Styled.rect
        [ Svg.Attributes.x (x + 3)
        , Svg.Attributes.y 3
        , Svg.Attributes.width ((range |> Range.duration |> Duration.toFloat |> (*) (width / numberOfHours)) - 6)
        , Svg.Attributes.height (dayHeight - 6)
        , Svg.Styled.Attributes.fill <| Code.color range.code
        , Svg.Attributes.rx 2
        , Svg.Attributes.ry 2
        ]
        []
    , Svg.Styled.text_
        [ Svg.Attributes.x (x + 10)
        , Svg.Attributes.y 20
        , Svg.Styled.Attributes.fill "black"
        ]
        [ Svg.Styled.text <| Code.toString range.code ]
    ]


ticks : Float -> List (Svg msg)
ticks height =
    List.range (ceiling from) (floor to)
        |> List.map toFloat
        |> List.map (\e -> e - from)
        |> List.map ((*) (width / numberOfHours))
        |> List.map (tick height)


tick : Float -> Float -> Svg msg
tick height x =
    Svg.Styled.line
        [ Svg.Attributes.x1 x
        , Svg.Attributes.y1 0
        , Svg.Attributes.x2 x
        , Svg.Attributes.y2 height
        , Svg.Styled.Attributes.stroke "darkgray"
        ]
        []



-- hourLabels : Float -> List (Svg msg)
-- hourLabels x =
--     List.range (ceiling from) (floor to)
--         |> List.map toFloat
--         |> List.map (\e -> e - from)
--         |> List.map ((*) (width / numberOfHours))
--         |> List.map (\e -> Svg.Styled.text_ [] [])


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
            , Css.left zero
            , Css.top zero
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
