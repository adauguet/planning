port module Edit exposing (Model, Msg, init, update, view)

import Code exposing (Code)
import Element
    exposing
        ( Attribute
        , Element
        , alignRight
        , below
        , clipY
        , column
        , el
        , fill
        , height
        , htmlAttribute
        , maximum
        , mouseOver
        , moveDown
        , padding
        , paddingXY
        , row
        , scrollbarY
        , spacing
        , text
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Json.Encode as E
import List.Extra
import Range exposing (Range)
import Tailwind exposing (white)
import Time exposing (Posix, Zone)
import Time.Helpers exposing (posixFromHoursMinutes)
import UI



-- model


type alias Model =
    { zone : Zone
    , posix : Posix
    , ranges : List Range
    , state : State
    , errorMessage : String
    }


type State
    = Default
    | EditBegin Int
    | EditEnd Int
    | EditCode Int


init : Zone -> Posix -> List Range -> Model
init zone posix ranges =
    { zone = zone
    , posix = posix
    , ranges =
        if List.isEmpty ranges then
            default zone posix

        else
            ranges
    , state = Default
    , errorMessage = ""
    }


default : Zone -> Posix -> List Range
default zone posix =
    [ { begin = posixFromHoursMinutes zone posix 8 30
      , end = posixFromHoursMinutes zone posix 12 30
      , code = Code.T
      }
    , { begin = posixFromHoursMinutes zone posix 14 0
      , end = posixFromHoursMinutes zone posix 17 30
      , code = Code.T
      }
    ]


newRange : Zone -> Posix -> Range
newRange zone posix =
    { begin = posixFromHoursMinutes zone posix 8 0
    , end = posixFromHoursMinutes zone posix 12 0
    , code = Code.T
    }



-- update


type Msg
    = ClickedBegin Int
    | ClickedEnd Int
    | ClickedCode Int
    | ClickedAddRange
    | ClickedDelete Int
    | DidSelectBeginning Int Posix
    | DidSelectEnd Int Posix
    | DidSelectCode Int Code


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedBegin index ->
            ( { model | state = EditBegin index }, scrollToSelectedOption )

        ClickedEnd index ->
            ( { model | state = EditEnd index }, scrollToSelectedOption )

        ClickedCode index ->
            ( { model | state = EditCode index }, Cmd.none )

        ClickedAddRange ->
            ( { model | ranges = model.ranges ++ [ newRange model.zone model.posix ] }, Cmd.none )

        ClickedDelete index ->
            ( { model | ranges = List.Extra.removeAt index model.ranges }, Cmd.none )

        DidSelectBeginning index posix ->
            ( { model | state = Default } |> updateRangeAt index (\r -> { r | begin = posix }), Cmd.none )

        DidSelectEnd index posix ->
            ( { model | state = Default } |> updateRangeAt index (\r -> { r | end = posix }), Cmd.none )

        DidSelectCode index code ->
            ( { model | state = Default } |> updateRangeAt index (\r -> { r | code = code }), Cmd.none )


updateRangeAt : Int -> (Range -> Range) -> Model -> Model
updateRangeAt index updateRange model =
    case List.Extra.getAt index model.ranges of
        Just range ->
            { model | ranges = List.Extra.setAt index (updateRange range) model.ranges }

        Nothing ->
            model


scrollToSelectedOption : Cmd Msg
scrollToSelectedOption =
    scrollTop "selected-option"


validateRanges : List Range -> Result String (List Range)
validateRanges ranges =
    validateEachRanges ranges
        |> Result.andThen validateConsecutiveRanges


validateEachRanges : List Range -> Result String (List Range)
validateEachRanges ranges =
    if List.all (\r -> Time.posixToMillis r.begin < Time.posixToMillis r.end) ranges then
        Ok ranges

    else
        Err "Erreur : pour chaque période, l'heure de fin doit être postérieure à l'heure de début."


validateConsecutiveRanges : List Range -> Result String (List Range)
validateConsecutiveRanges ranges =
    let
        validate r =
            case r of
                first :: second :: tail ->
                    Time.posixToMillis first.end <= Time.posixToMillis second.begin && validate (second :: tail)

                _ ->
                    True
    in
    if validate ranges then
        Ok ranges

    else
        Err "Erreur : les périodes ne doivent pas se chevaucher."


port scrollTop_ : E.Value -> Cmd msg


scrollTop : String -> Cmd msg
scrollTop id =
    E.string id |> scrollTop_



-- view


view : (Msg -> msg) -> msg -> (Result String (List Range) -> msg) -> Model -> Element msg
view parentMsg clickedCancel clickedValidate model =
    column
        [ spacing 8, Font.size 12 ]
        [ column [ spacing 4 ]
            (model.ranges
                |> List.indexedMap (rangeView model)
                |> List.map (Element.map parentMsg)
            )
        , Input.button buttonAttributes { onPress = Just <| parentMsg ClickedAddRange, label = text "Ajouter une plage" }
        , el [] <| text model.errorMessage
        , row [ alignRight, spacing 4 ]
            [ Input.button buttonAttributes { onPress = Just clickedCancel, label = text "Annuler" }
            , Input.button buttonAttributes { onPress = Just <| clickedValidate <| validateRanges model.ranges, label = text "Valider" }
            ]
        ]


rangeView : Model -> Int -> Range -> Element Msg
rangeView model index range =
    row [ spacing 4 ]
        [ el (ifIndex model.state EditBegin index <| timeSelect model.zone model.posix range.begin (DidSelectBeginning index)) <|
            Input.button inputAttributes
                { onPress = Just <| ClickedBegin index
                , label = text <| Time.Helpers.hourMinuteString model.zone range.begin
                }
        , el (ifIndex model.state EditEnd index <| timeSelect model.zone model.posix range.end (DidSelectEnd index)) <|
            Input.button inputAttributes
                { onPress = Just <| ClickedEnd index
                , label = text <| Time.Helpers.hourMinuteString model.zone range.end
                }
        , el (ifIndex model.state EditCode index <| codeSelect index range.code) <|
            Input.button inputAttributes
                { onPress = Just <| ClickedCode index
                , label = text <| Code.description range.code
                }
        , Input.button buttonAttributes { onPress = Just <| ClickedDelete index, label = text "Supprimer" }
        ]


ifIndex : State -> (Int -> State) -> Int -> Element msg -> List (Attribute msg)
ifIndex state toState index element =
    if state == toState index then
        [ below element ]

    else
        []


codeSelect : Int -> Code -> Element Msg
codeSelect index code =
    let
        option c =
            Input.option c (text <| Code.description c)
    in
    Input.radio radioAttributes
        { onChange = DidSelectCode index
        , options = List.map option Code.selectList
        , selected = Just code
        , label = Input.labelHidden "Code"
        }


timeSelect : Zone -> Posix -> Posix -> (Posix -> Msg) -> Element Msg
timeSelect zone today time onChange =
    let
        option posix =
            Input.optionWith posix
                (\state ->
                    case state of
                        Input.Idle ->
                            el
                                [ padding 8
                                , mouseOver [ Background.color Tailwind.gray200 ]
                                ]
                            <|
                                text <|
                                    Time.Helpers.hourMinuteString zone posix

                        Input.Focused ->
                            el
                                [ padding 8
                                , mouseOver [ Background.color Tailwind.gray200 ]
                                ]
                            <|
                                text <|
                                    Time.Helpers.hourMinuteString zone posix

                        Input.Selected ->
                            el
                                [ Font.heavy
                                , htmlAttribute <| Html.Attributes.id "selected-option"
                                , padding 8
                                , mouseOver [ Background.color Tailwind.gray200 ]
                                ]
                                (text <| Time.Helpers.hourMinuteString zone posix)
                )

        ticks =
            let
                mod a b =
                    ( b // a, modBy a b )
            in
            List.range 0 ((19 - 7) * 4)
                |> List.map ((*) 15)
                |> List.map (mod 60)
                |> List.map (Tuple.mapFirst ((+) 7))
                |> List.map (\( h, m ) -> posixFromHoursMinutes zone today h m)
    in
    Input.radio radioAttributes
        { onChange = onChange
        , options = List.map option ticks
        , selected = Just time
        , label = Input.labelHidden ""
        }


radioAttributes : List (Attribute msg)
radioAttributes =
    [ paddingXY 0 8
    , Background.color white
    , UI.shadow
    , height (fill |> maximum 200)
    , clipY
    , scrollbarY
    , moveDown 4
    ]


buttonAttributes : List (Attribute msg)
buttonAttributes =
    [ padding 8, Border.width 1 ]


inputAttributes : List (Attribute msg)
inputAttributes =
    [ padding 8, Background.color Tailwind.gray300 ]
