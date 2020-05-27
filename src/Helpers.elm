module Helpers exposing (chunks)


chunks : Int -> List a -> List (List a)
chunks count list =
    case ( List.take count list, List.drop count list ) of
        ( [], _ ) ->
            []

        ( take, drop ) ->
            take :: chunks count drop
