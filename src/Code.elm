module Code exposing (Code(..), backgroundColor, backgroundColorE, color, colorE, comment, decoder, description, isPaid, selectList, toString)

import Css exposing (Color)
import Element exposing (rgb255)
import Json.Decode as D exposing (Decoder)


type Code
    = T
    | TT
    | HS
    | NT
    | RCR
    | CP
    | AT
    | F
    | JS
    | AAP
    | AGE


selectList : List Code
selectList =
    [ T, TT, HS, NT, RCR, CP, AT, AAP, AGE ]


toString : Code -> String
toString code =
    case code of
        T ->
            "T"

        TT ->
            "TT"

        HS ->
            "HS"

        NT ->
            "NT"

        RCR ->
            "RCR"

        CP ->
            "CP"

        AT ->
            "AT"

        F ->
            "F"

        JS ->
            "JS"

        AAP ->
            "AAP"

        AGE ->
            "AGE"


description : Code -> String
description code =
    case code of
        T ->
            "Heures travaillées"

        TT ->
            "Heures télétravaillées"

        HS ->
            "Heures supplémentaires"

        NT ->
            "Non Travaillées (et de professionnalisation)"

        RCR ->
            "Repos Compensateur de Récupération"

        CP ->
            "Congés Payés"

        AT ->
            "Arrêt de Travail"

        F ->
            "Férié"

        JS ->
            "Journée de Solidarité"

        AAP ->
            "Arrêt Activité Partielle"

        AGE ->
            "Arrêt Garde Enfants"


comment : Code -> String
comment code =
    case code of
        NT ->
            "Cf contrat travail ou temps aménagé"

        _ ->
            ""


backgroundColor : Code -> Color
backgroundColor code =
    case code of
        T ->
            Css.hex "4285F4"

        TT ->
            Css.hex "4285F4"

        HS ->
            Css.hex "4285F4"

        AAP ->
            Css.hex "E1E1E1"

        AT ->
            Css.hex "E1E1E1"

        NT ->
            Css.hex "E1E1E1"

        _ ->
            Css.hex "63DA38"


backgroundColorE : Code -> Element.Color
backgroundColorE code =
    case code of
        T ->
            rgb255 66 133 244

        TT ->
            rgb255 66 133 244

        HS ->
            rgb255 66 133 244

        AAP ->
            rgb255 225 225 225

        AT ->
            rgb255 225 225 225

        NT ->
            rgb255 225 225 225

        _ ->
            rgb255 99 218 56


color : Code -> Color
color code =
    case code of
        T ->
            Css.hex "FFFFFF"

        TT ->
            Css.hex "FFFFFF"

        HS ->
            Css.hex "FFFFFF"

        _ ->
            Css.hex "3C4043"


colorE : Code -> Element.Color
colorE code =
    case code of
        T ->
            rgb255 255 255 255

        TT ->
            rgb255 255 255 255

        HS ->
            rgb255 255 255 255

        _ ->
            rgb255 60 64 67


isPaid : Code -> Bool
isPaid code =
    case code of
        T ->
            True

        TT ->
            True

        F ->
            True

        AGE ->
            True

        AT ->
            True

        CP ->
            True

        RCR ->
            True

        HS ->
            True

        JS ->
            True

        NT ->
            False

        AAP ->
            False


decoder : Decoder Code
decoder =
    D.string
        |> D.andThen
            (\string ->
                case fromString string of
                    Just code ->
                        D.succeed code

                    Nothing ->
                        D.fail <| "could not decode code: " ++ string
            )


fromString : String -> Maybe Code
fromString string =
    case string of
        "T" ->
            Just T

        "TT" ->
            Just TT

        "HS" ->
            Just HS

        "NT" ->
            Just NT

        "RCR" ->
            Just RCR

        "CP" ->
            Just CP

        "AT" ->
            Just AT

        "F" ->
            Just F

        "JS" ->
            Just JS

        "AAP" ->
            Just AAP

        "AGE" ->
            Just AGE

        _ ->
            Nothing
