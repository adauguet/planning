module Range exposing (Model, Msg, init, update, view)

import Array
import Code exposing (Code)
import Css
import Html.Styled exposing (Html, div, option, select, text)
import Html.Styled.Attributes exposing (css, selected)
import Html.Styled.Events exposing (on)
import Json.Decode as D exposing (Decoder)
import Time exposing (Posix)



-- model


type alias Model =
    { begin : Posix
    , end : Posix
    , code : Code
    }


init : Model
init =
    { begin = Time.millisToPosix 0
    , end = Time.millisToPosix 0
    , code = { code = "T", description = "Heures travaillées", comment = "" }
    }



-- update


type Msg
    = DidSelectBeginning ( Int, Int )
    | DidSelectEnd ( Int, Int )
    | DidSelectCode Code


update : Msg -> Model -> Model
update msg model =
    case msg of
        DidSelectEnd ( _, _ ) ->
            model

        DidSelectBeginning ( _, _ ) ->
            model

        DidSelectCode _ ->
            model



-- view


view : Model -> Html Msg
view model =
    div [ css [ Css.displayFlex ] ]
        [ div [ css [ Css.displayFlex, Css.flexDirection Css.column ] ]
            [ timeSelect ticks ( 7, 0 ) DidSelectBeginning
            ]
        , div [ css [ Css.displayFlex, Css.flexDirection Css.column ] ]
            [ timeSelect ticks ( 7, 0 ) DidSelectEnd
            ]
        , div [ css [ Css.displayFlex, Css.flexDirection Css.column ] ]
            [ codeSelect Code.defaultCodes model.code
            ]
        ]


codeSelect : List Code -> Code -> Html Msg
codeSelect codes selectedCode =
    let
        optionView code =
            option [ selected (code == selectedCode) ] [ text code.description ]
    in
    select [ on "input" (targetSelectedIndex codes DidSelectCode) ] (List.map optionView codes)


targetSelectedIndex : List a -> (a -> msg) -> Decoder msg
targetSelectedIndex items msg =
    D.at [ "target", "selectedIndex" ] D.int
        |> D.andThen
            (\index ->
                case
                    items
                        |> Array.fromList
                        |> Array.get index
                of
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
