module Day.Kind exposing (Kind(..), decoder, encode)

import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Range exposing (Range)
import Time exposing (Posix)


type Kind
    = Working (List Range)
    | Holiday
    | Solidarity


encode : (Posix -> Value) -> Kind -> List ( String, Value )
encode encodePosix kind =
    case kind of
        Working ranges ->
            [ ( "kind", E.string "default" )
            , ( "ranges", E.list (Range.encode encodePosix) ranges )
            ]

        Holiday ->
            [ ( "kind", E.string "holiday" ) ]

        Solidarity ->
            [ ( "kind", E.string "solidarity" ) ]


decoder : Decoder Kind
decoder =
    D.string
        |> D.andThen
            (\kind ->
                case kind of
                    "default" ->
                        D.field "ranges" (D.list Range.decoder)
                            |> D.andThen
                                (\list ->
                                    D.succeed <| Working list
                                )

                    "holiday" ->
                        D.succeed Holiday

                    "solidarity" ->
                        D.succeed Solidarity

                    other ->
                        D.fail <| "could not decode kind: " ++ other
            )
