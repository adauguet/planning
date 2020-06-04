module Helpers exposing (chunks, rangeStep)


chunks : Int -> List a -> List (List a)
chunks count list =
    case ( List.take count list, List.drop count list ) of
        ( [], _ ) ->
            []

        ( take, drop ) ->
            take :: chunks count drop


rangeStep : Int -> Int -> Int -> List Int
rangeStep from to step =
    rangeStep_ from to step []


rangeStep_ : Int -> Int -> Int -> List Int -> List Int
rangeStep_ from to step list =
    if from <= to then
        rangeStep_ (from + step) to step (list ++ [ from ])

    else
        list
