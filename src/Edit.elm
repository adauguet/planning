module Edit exposing (Model, Msg, init, update, view)

import Code exposing (Code)
import Css
import Day exposing (Kind(..))
import Html.Styled exposing (Html, button, div, input, option, select, text)
import Html.Styled.Attributes exposing (checked, css, disabled, selected, type_)
import Html.Styled.Events exposing (on, onCheck, onClick)
import Json.Decode as D exposing (Decoder)
import List.Extra
import Range exposing (Range)



-- model


type alias Model =
    { ranges : List Range
    , dayType : DayType
    , errorMessage : String
    }


type DayType
    = Default
    | Holiday
    | Solidarity


computeDayKind : Model -> Result String Day.Kind
computeDayKind model =
    case model.dayType of
        Default ->
            validateRanges model.ranges
                |> Result.map Day.Default

        Holiday ->
            Ok Day.Holiday

        Solidarity ->
            Ok Day.Solidarity


init : Kind -> Model
init kind =
    case kind of
        Day.Default [] ->
            { ranges = default
            , dayType = Default
            , errorMessage = ""
            }

        Day.Default ranges ->
            { ranges = ranges
            , dayType = Default
            , errorMessage = ""
            }

        Day.Holiday ->
            { ranges = []
            , dayType = Holiday
            , errorMessage = ""
            }

        Day.Solidarity ->
            { ranges = []
            , dayType = Solidarity
            , errorMessage = ""
            }


default : List Range
default =
    [ { begin = Range.Time ( 7, 30 )
      , end = Range.Time ( 8, 30 )
      , code = Code.T
      }
    , { begin = Range.Time ( 8, 30 )
      , end = Range.Time ( 10, 0 )
      , code = Code.TT
      }
    , { begin = Range.Time ( 10, 0 )
      , end = Range.Time ( 11, 30 )
      , code = Code.HS
      }
    , { begin = Range.Time ( 11, 30 )
      , end = Range.Time ( 13, 0 )
      , code = Code.NT
      }
    , { begin = Range.Time ( 13, 0 )
      , end = Range.Time ( 14, 30 )
      , code = Code.RCR
      }
    , { begin = Range.Time ( 14, 30 )
      , end = Range.Time ( 16, 0 )
      , code = Code.AGE
      }
    , { begin = Range.Time ( 16, 0 )
      , end = Range.Time ( 17, 30 )
      , code = Code.AT
      }
    , { begin = Range.Time ( 17, 30 )
      , end = Range.Time ( 19, 0 )
      , code = Code.AAP
      }
    ]



-- [ { begin = ( 8, 30 )
--   , end = ( 12, 30 )
--   , code = Code.T
--   }
-- , { begin = ( 14, 0 )
--   , end = ( 18, 0 )
--   , code = Code.TT
--   }
-- ]


newRange : Range
newRange =
    { begin = Range.Time ( 8, 0 )
    , end = Range.Time ( 12, 0 )
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
    = DidSelectBeginning Range.Time
    | DidSelectEnd Range.Time
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
            ( { model | ranges = model.ranges ++ [ newRange ] }, Cmd.none )

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
    if List.all (\r -> Range.toMinutes r.begin < Range.toMinutes r.end) ranges then
        Ok ranges

    else
        Err "Erreur : pour chaque période, l'heure de fin doit être postérieure à l'heure de début."


validateConsecutiveRanges : List Range -> Result String (List Range)
validateConsecutiveRanges ranges =
    let
        validate r =
            case r of
                first :: second :: tail ->
                    Range.toMinutes first.end <= Range.toMinutes second.begin && validate (second :: tail)

                _ ->
                    True
    in
    if validate ranges then
        Ok ranges

    else
        Err "Erreur : les périodes ne doivent pas se chevaucher."



-- view


view : (Msg -> msg) -> msg -> (Result String Day.Kind -> msg) -> Model -> Html msg
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
                |> List.indexedMap (rangeView (model.dayType /= Default))
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


rangeView : Bool -> Int -> Range -> Html Msg
rangeView isDisabled index range =
    div [ css [ Css.displayFlex ] ]
        [ div [ css [ Css.displayFlex, Css.flexDirection Css.column ] ]
            [ timeSelect isDisabled ticks range.begin (DidSelectBeginning >> OnRangeMsg index)
            ]
        , div [ css [ Css.displayFlex, Css.flexDirection Css.column ] ]
            [ timeSelect isDisabled ticks range.end (DidSelectEnd >> OnRangeMsg index)
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


ticks : List Range.Time
ticks =
    let
        mod a b =
            ( b // a, modBy a b )
    in
    List.range 0 ((19 - 7) * 4)
        |> List.map ((*) 15)
        |> List.map (mod 60)
        |> List.map (Tuple.mapFirst ((+) 7))
        |> List.map Range.Time


timeSelect : Bool -> List Range.Time -> Range.Time -> (Range.Time -> msg) -> Html msg
timeSelect isDisabled times (Range.Time selectedTime) msg =
    let
        timeView (Range.Time ( hours, minutes )) =
            option [ selected (( hours, minutes ) == selectedTime) ]
                [ text
                    (String.padLeft 2 '0' (String.fromInt hours)
                        ++ ":"
                        ++ String.padLeft 2 '0' (String.fromInt minutes)
                    )
                ]
    in
    select
        [ on "input" (targetSelectedIndex times msg)
        , disabled isDisabled
        ]
        (List.map timeView times)
