module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Code
import Css exposing (pct, px, rem, zero)
import Date
import Day exposing (Day)
import Derberos.Date.Calendar exposing (getCurrentMonthDates)
import Edit
import FormatDate exposing (formatDate, monthString)
import Html.Styled exposing (Attribute, Html, div, span, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
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
    | ClickedValidate Int (List Range)


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
                , days = generateDays here now
              }
            , Cmd.none
            )

        ClickedDay index day ->
            ( { model | state = Edit index <| Edit.init day.ranges }, Cmd.none )

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

        ClickedValidate index ranges ->
            case validateRanges ranges of
                Ok validatedRanges ->
                    let
                        days =
                            case List.Extra.getAt index model.days of
                                Just day ->
                                    List.Extra.setAt index { day | ranges = validatedRanges } model.days

                                Nothing ->
                                    model.days
                    in
                    ( { model | state = Initial, days = days }, Cmd.none )

                Err _ ->
                    -- let
                    --     _ =
                    --         Debug.log "error message" message
                    -- in
                    ( model, Cmd.none )


generateDays : Zone -> Posix -> List Day
generateDays zone current =
    getCurrentMonthDates zone current
        |> List.map (\d -> { date = d, ranges = [] })


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


validateRanges : List Range -> Result String (List Range)
validateRanges ranges =
    validateEachRanges ranges
        |> Result.andThen validateConsecutiveRanges


validateEachRanges : List Range -> Result String (List Range)
validateEachRanges ranges =
    if List.all (\r -> r.begin < r.end) ranges then
        Ok ranges

    else
        Err "Erreur : pour chaque période, l'heure de fin doit être postérieure à l'heure de début."


validateConsecutiveRanges : List Range -> Result String (List Range)
validateConsecutiveRanges ranges =
    let
        validate r =
            case r of
                first :: second :: tail ->
                    first.end <= second.begin && validate (second :: tail)

                _ ->
                    True
    in
    if validate ranges then
        Ok ranges

    else
        Err "Erreur : les périodes ne doivent pas se chevaucher."



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
            div [] (monthHeader zone time :: List.map (weekView zone time) weeks)

        _ ->
            text ""
    , case model.state of
        Initial ->
            text ""

        Edit index subModel ->
            modal [] [] [ Edit.view EditMsg ClickedCancel (ClickedValidate index) subModel ]
    ]


weekView : Zone -> Posix -> ( Int, List ( Int, Day ) ) -> Html Msg
weekView zone currentDate ( n, days ) =
    div [ css [ Css.displayFlex ] ]
        [ div [ css [ Css.padding (rem 1) ] ] [ text <| "Semaine " ++ String.fromInt n ]
        , div [ css [ Css.padding (rem 1) ] ] (List.map (dayView zone currentDate) days)
        ]


monthHeader : Zone -> Posix -> Html msg
monthHeader zone posix =
    div
        [ css
            [ Css.displayFlex
            , Css.alignItems Css.center
            , Css.padding (rem 1)
            ]
        ]
        [ div
            [ css
                [ Css.fontSize (rem 1.5)
                , Css.fontWeight (Css.int 900)
                ]
            ]
            [ text <| monthString zone posix
            , span
                [ css
                    [ Css.fontSize (rem 1.5)
                    , Css.marginLeft (rem 0.5)
                    ]
                ]
                [ text <| String.fromInt <| Time.toYear zone posix ]
            ]
        ]


dayView : Zone -> Posix -> ( Int, Day ) -> Html Msg
dayView zone today ( index, day ) =
    div
        [ css
            [ Css.displayFlex

            -- , Css.margin (rem 0.2)
            -- , Css.padding (rem 0.25)
            , Css.textAlign Css.end
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
        [ div [ css [ Css.minWidth (Css.px 150) ] ] [ text <| formatDate zone day.date ]
        , div [ css [ Css.marginLeft (rem 1) ] ] [ planningView day.ranges ]
        ]


width : Float
width =
    1000


height : Float
height =
    25


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
        , Svg.Attributes.height height
        , Svg.Attributes.viewBox 0 0 width height
        ]
        (frame
            :: ticks
            ++ (List.map rangeSvg ranges |> List.concat)
        )


frame : Svg msg
frame =
    Svg.Styled.rect
        [ Svg.Attributes.x 0
        , Svg.Attributes.y 0
        , Svg.Attributes.width width
        , Svg.Attributes.height height
        , Svg.Attributes.rx 5
        , Svg.Attributes.ry 5
        , Svg.Styled.Attributes.fill "#EDF2F7"
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
        , Svg.Attributes.y 0
        , Svg.Attributes.width ((Range.duration range |> (*) (width / numberOfHours)) - 6)
        , Svg.Attributes.height height
        , Svg.Styled.Attributes.fill <| Code.color range.code
        , Svg.Attributes.rx 2
        , Svg.Attributes.ry 2
        ]
        []
    , Svg.Styled.text_
        [ Svg.Attributes.x (x + 10)
        , Svg.Attributes.y 17
        , Svg.Styled.Attributes.fill "black"
        ]
        [ Svg.Styled.text <| Code.toString range.code ]
    ]


ticks : List (Svg msg)
ticks =
    List.range (ceiling from) (floor to)
        |> List.map toFloat
        |> List.map (\e -> e - from)
        |> List.map ((*) (width / numberOfHours))
        |> List.map tick


tick : Float -> Svg msg
tick x =
    Svg.Styled.line
        [ Svg.Attributes.x1 x
        , Svg.Attributes.y1 0
        , Svg.Attributes.x2 x
        , Svg.Attributes.y2 height
        , Svg.Styled.Attributes.stroke "black"
        ]
        []


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
