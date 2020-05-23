module Code exposing (Code, defaultCodes)


type alias Code =
    { code : String
    , description : String
    , comment : String
    }


defaultCodes : List Code
defaultCodes =
    [ { code = "T", description = "Heures travaillées", comment = "" }
    , { code = "AT", description = "Arrêt de Travail", comment = "" }
    , { code = "TT", description = "Heures télétravaillées", comment = "" }
    , { code = "CP", description = "Congés Payés", comment = "" }
    , { code = "AAP", description = "Arrêt Activité Partielle", comment = "" }
    , { code = "RCR", description = "Repos Compensateur de Récupération", comment = "" }
    , { code = "F", description = "Férié", comment = "" }
    , { code = "NT", description = "Non Travaillées (et de professionnalisation)", comment = "Cf contrat travail ou temps aménagé" }
    , { code = "AGE", description = "Arrêt Garde Enfants", comment = "" }
    ]
