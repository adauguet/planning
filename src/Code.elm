module Code exposing (Code(..), color, comment, description, selectList, toString)


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


color : Code -> String
color code =
    case code of
        T ->
            "#4299E1"

        TT ->
            "#63B3ED"

        HS ->
            "#3182CE"

        _ ->
            ""
