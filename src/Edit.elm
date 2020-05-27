module Edit exposing (Model, Msg, init, update, view)

import Code exposing (Code)
import Css
import Html.Styled exposing (Html, button, div, option, select, text)
import Html.Styled.Attributes exposing (css, selected)
import Html.Styled.Events exposing (on, onClick)
import Json.Decode as D exposing (Decoder)
import List.Extra
import Range exposing (Range)



-- model


type alias Model =
    { ranges : List Range
    , errorMessage : String
    }


init : List Range -> Model
init ranges =
    { ranges =
        if List.isEmpty ranges then
            [ { begin = ( 8, 30 )
              , end = ( 12, 30 )
              , code = Code.T
              }
            , { begin = ( 14, 0 )
              , end = ( 18, 0 )
              , code = Code.TT
              }
            ]

        else
            ranges
    , errorMessage = ""
    }


newRange : Range
newRange =
    { begin = ( 8, 0 )
    , end = ( 12, 0 )
    , code = Code.T
    }



-- update


type Msg
    = OnRangeMsg Int RangeMsg
    | ClickedAddRange
    | ClickedDelete Int


type RangeMsg
    = DidSelectBeginning ( Int, Int )
    | DidSelectEnd ( Int, Int )
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


updateRange : RangeMsg -> Range -> Range
updateRange msg model =
    case msg of
        DidSelectBeginning begin ->
            { model | begin = begin }

        DidSelectEnd end ->
            { model | end = end }

        DidSelectCode code ->
            { model | code = code }



-- view


view : (Msg -> msg) -> msg -> (List Range -> msg) -> Model -> Html msg
view parentMsg clickedCancel clickedValidate model =
    div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.alignItems Css.flexStart
            ]
        ]
        [ div []
            (model.ranges
                |> List.indexedMap rangeView
                |> List.map (Html.Styled.map parentMsg)
            )
        , button [ onClick <| parentMsg ClickedAddRange ] [ text "Ajouter une plage" ]
        , div [] [ text model.errorMessage ]
        , div [ css [ Css.alignSelf Css.flexEnd ] ]
            [ button [ onClick clickedCancel ] [ text "Annuler" ]
            , button [ onClick <| clickedValidate model.ranges ] [ text "Valider" ]
            ]
        ]


rangeView : Int -> Range -> Html Msg
rangeView index range =
    div [ css [ Css.displayFlex ] ]
        [ div [ css [ Css.displayFlex, Css.flexDirection Css.column ] ]
            [ timeSelect ticks range.begin (DidSelectBeginning >> OnRangeMsg index)
            ]
        , div [ css [ Css.displayFlex, Css.flexDirection Css.column ] ]
            [ timeSelect ticks range.end (DidSelectEnd >> OnRangeMsg index)
            ]
        , div [ css [ Css.displayFlex, Css.flexDirection Css.column ] ]
            [ codeSelect index Code.selectList range.code
            ]
        , div [] [ button [ onClick (ClickedDelete index) ] [ text "Supprimer" ] ]
        ]


codeSelect : Int -> List Code -> Code -> Html Msg
codeSelect index codes selectedCode =
    let
        optionView code =
            option [ selected (code == selectedCode) ] [ text <| Code.description code ]
    in
    select [ on "input" (targetSelectedIndex codes (DidSelectCode >> OnRangeMsg index)) ] (List.map optionView codes)


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


ticks : List ( Int, Int )
ticks =
    let
        mod a b =
            ( b // a, modBy a b )
    in
    List.range 0 ((19 - 7) * 4)
        |> List.map ((*) 15)
        |> List.map (mod 60)
        |> List.map (Tuple.mapFirst ((+) 7))


timeSelect : List ( Int, Int ) -> ( Int, Int ) -> (( Int, Int ) -> msg) -> Html msg
timeSelect times selectedTime msg =
    let
        timeView ( hours, minutes ) =
            option [ selected (( hours, minutes ) == selectedTime) ]
                [ text
                    (String.padLeft 2 '0' (String.fromInt hours)
                        ++ ":"
                        ++ String.padLeft 2 '0' (String.fromInt minutes)
                    )
                ]
    in
    select [ on "input" (targetSelectedIndex times msg) ] (List.map timeView times)
