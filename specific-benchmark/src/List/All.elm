module List.All exposing (notAnyNot, notAnyNotComposition, notAnyNotFromLet, recursive)


notAnyNot : (a -> Bool) -> List a -> Bool
notAnyNot isOkay list =
    not (List.any (\a -> not (isOkay a)) list)


notAnyNotFromLet : (a -> Bool) -> List a -> Bool
notAnyNotFromLet isOkay list =
    let
        isBad : a -> Bool
        isBad a =
            not (isOkay a)
    in
    not (List.any isBad list)


notAnyNotComposition : (a -> Bool) -> List a -> Bool
notAnyNotComposition isOkay list =
    not (List.any (not << isOkay) list)


{-| This version is used by [eol2](https://github.com/mdgriffith/elm-optimize-level-2/pull/65)
-}
recursive : (a -> Bool) -> List a -> Bool
recursive isOkay list =
    case list of
        [] ->
            True

        head :: tail ->
            -- note: && does not get TCO
            if isOkay head then
                recursive isOkay tail

            else
                False
