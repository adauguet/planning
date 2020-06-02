module Code exposing (Code(..), backgroundColor, color, comment, description, isPaid, selectList, toString)

import Css exposing (Color)


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
