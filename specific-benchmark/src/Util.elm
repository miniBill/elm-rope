module Util exposing (listAll, listFilledFoldl1Map, listReverseMap, listSumMap)


listSumMap : (a -> number) -> List a -> number
listSumMap toNumber list =
    List.foldl (\el acc -> acc + toNumber el) 0 list


listReverseMap : (a -> b) -> List a -> List b
listReverseMap elementChange list =
    List.foldl (\el acc -> elementChange el :: acc) [] list


listFilledFoldl1Map : (a -> b) -> (b -> b -> b) -> ( a, List a ) -> b
listFilledFoldl1Map change reduce ( el0, el1Up ) =
    el1Up
        |> List.foldl
            (\el soFar ->
                reduce (change el) soFar
            )
            (change el0)


{-| Optimized version of List.all used by [eol2](https://github.com/mdgriffith/elm-optimize-level-2/pull/65)
-}
listAll : (a -> Bool) -> List a -> Bool
listAll isOkay list =
    case list of
        [] ->
            True

        head :: tail ->
            -- note: && does not get TCO
            if isOkay head then
                listAll isOkay tail

            else
                False
