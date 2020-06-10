module Edit exposing (Model, Msg, init, update, view)

import Code exposing (Code)
import Css
import Day.Kind exposing (Kind)
import Html.Styled exposing (Html, button, div, input, option, select, text)
import Html.Styled.Attributes exposing (checked, css, disabled, selected, type_)
import Html.Styled.Events exposing (on, onCheck, onClick)
import Json.Decode as D exposing (Decoder)
import List.Extra
import Range exposing (Range)
import Time exposing (Posix, Zone)
import Time.Helpers exposing (posixFromHoursMinutes)



-- model


type alias Model =
    { zone : Zone
    , posix : Posix
    , ranges : List Range
    , dayType : DayType
    , errorMessage : String
    }


type DayType
    = Default
    | Holiday
    | Solidarity


computeDayKind : Model -> Result String Kind
computeDayKind model =
    case model.dayType of
        Default ->
            validateRanges model.ranges
                |> Result.map Day.Kind.Working

        Holiday ->
            Ok Day.Kind.Holiday

        Solidarity ->
            Ok Day.Kind.Solidarity


init : Zone -> Posix -> Kind -> Model
init zone posix kind =
    case kind of
        Day.Kind.Working [] ->
            { zone = zone
            , posix = posix
            , ranges = default zone posix
            , dayType = Default
            , errorMessage = ""
            }

        Day.Kind.Working ranges ->
            { zone = zone
            , posix = posix
            , ranges = ranges
            , dayType = Default
            , errorMessage = ""
            }

        Day.Kind.Holiday ->
            { zone = zone
            , posix = posix
            , ranges = []
            , dayType = Holiday
            , errorMessage = ""
            }

        Day.Kind.Solidarity ->
            { zone = zone
            , posix = posix
            , ranges = []
            , dayType = Solidarity
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
    = OnRangeMsg Int RangeMsg
    | ClickedAddRange
    | ClickedDelete Int
    | DidCheckHoliday Bool
    | DidCheckSolidarityDay Bool


type RangeMsg
    = DidSelectBeginning Posix
    | DidSelectEnd Posix
    | DidSelectCode Code


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnRangeMsg index subMsg ->
            let
                subModel =
                    List.Extra.getAt index model.ranges
            in
            case subModel of
                Just m ->
                    ( { model | ranges = List.Extra.setAt index (updateRange subMsg m) model.ranges }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ClickedAddRange ->
            ( { model | ranges = model.ranges ++ [ newRange model.zone model.posix ] }, Cmd.none )

        ClickedDelete index ->
            ( { model | ranges = List.Extra.removeAt index model.ranges }, Cmd.none )

        DidCheckHoliday isChecked ->
            ( { model
                | dayType =
                    if isChecked then
                        Holiday

                    else
                        Default
              }
            , Cmd.none
            )

        DidCheckSolidarityDay isChecked ->
            ( { model
                | dayType =
                    if isChecked then
                        Solidarity

                    else
                        Default
              }
            , Cmd.none
            )


updateRange : RangeMsg -> Range -> Range
updateRange msg model =
    case msg of
        DidSelectBeginning begin ->
            { model | begin = begin }

        DidSelectEnd end ->
            { model | end = end }

        DidSelectCode code ->
            { model | code = code }


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



-- view


view : (Msg -> msg) -> msg -> (Result String Kind -> msg) -> Model -> Html msg
view parentMsg clickedCancel clickedValidate model =
    div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.alignItems Css.flexStart
            ]
        ]
        [ holiday (model.dayType == Holiday) |> Html.Styled.map parentMsg
        , solidarityDay (model.dayType == Solidarity) |> Html.Styled.map parentMsg
        , div []
            (model.ranges
                |> List.indexedMap (rangeView model.zone model.posix (model.dayType /= Default))
                |> List.map (Html.Styled.map parentMsg)
            )
        , button [ onClick <| parentMsg ClickedAddRange, disabled (model.dayType /= Default) ] [ text "Ajouter une plage" ]
        , div [] [ text model.errorMessage ]
        , div [ css [ Css.alignSelf Css.flexEnd ] ]
            [ button [ onClick clickedCancel ] [ text "Annuler" ]
            , button [ onClick <| clickedValidate <| computeDayKind model ] [ text "Valider" ]
            ]
        ]


holiday : Bool -> Html Msg
holiday isChecked =
    div []
        [ input
            [ type_ "checkbox"
            , checked isChecked
            , onCheck DidCheckHoliday
            ]
            []
        , text "Férié"
        ]


solidarityDay : Bool -> Html Msg
solidarityDay isChecked =
    div []
        [ input
            [ type_ "checkbox"
            , checked isChecked
            , onCheck DidCheckSolidarityDay
            ]
            []
        , text "Journée de solidarité"
        ]


rangeView : Zone -> Posix -> Bool -> Int -> Range -> Html Msg
rangeView zone posix isDisabled index range =
    div [ css [ Css.displayFlex ] ]
        [ div [ css [ Css.displayFlex, Css.flexDirection Css.column ] ]
            [ timeSelect zone isDisabled (ticks zone posix) range.begin (DidSelectBeginning >> OnRangeMsg index)
            ]
        , div [ css [ Css.displayFlex, Css.flexDirection Css.column ] ]
            [ timeSelect zone isDisabled (ticks zone posix) range.end (DidSelectEnd >> OnRangeMsg index)
            ]
        , div [ css [ Css.displayFlex, Css.flexDirection Css.column ] ]
            [ codeSelect isDisabled index Code.selectList range.code
            ]
        , div []
            [ button
                [ onClick (ClickedDelete index)
                , disabled isDisabled
                ]
                [ text "Supprimer" ]
            ]
        ]


codeSelect : Bool -> Int -> List Code -> Code -> Html Msg
codeSelect isDisabled index codes selectedCode =
    let
        optionView code =
            option [ selected (code == selectedCode) ] [ text <| Code.description code ]
    in
    select
        [ on "input" (targetSelectedIndex codes (DidSelectCode >> OnRangeMsg index))
        , disabled isDisabled
        ]
        (List.map optionView codes)


targetSelectedIndex : List a -> (a -> msg) -> Decoder msg
targetSelectedIndex items msg =
    D.at [ "target", "selectedIndex" ] D.int
        |> D.andThen
            (\index ->
                case List.Extra.getAt index items of
                    Just item ->
                        D.succeed (msg item)

                    Nothing ->
                        D.fail "could not decode selected item"
            )


ticks : Zone -> Posix -> List Posix
ticks zone posix =
    let
        mod a b =
            ( b // a, modBy a b )
    in
    List.range 0 ((19 - 7) * 4)
        |> List.map ((*) 15)
        |> List.map (mod 60)
        |> List.map (Tuple.mapFirst ((+) 7))
        |> List.map (\( h, m ) -> posixFromHoursMinutes zone posix h m)


timeSelect : Zone -> Bool -> List Posix -> Posix -> (Posix -> msg) -> Html msg
timeSelect zone isDisabled times selectedTime msg =
    let
        timeView time =
            option
                [ selected (time == selectedTime) ]
                [ text <| Time.Helpers.hourMinuteString zone time ]
    in
    select
        [ on "input" (targetSelectedIndex times msg)
        , disabled isDisabled
        ]
        (List.map timeView times)
