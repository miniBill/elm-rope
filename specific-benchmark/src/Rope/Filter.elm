module Rope.Filter exposing (foldlReverseToFlat, foldlToEndToFlat, foldrToFlat, keepNested)

import Rope.Local as Rope exposing (Rope)


foldrToFlat : (a -> Bool) -> Rope a -> Rope a
foldrToFlat isGood rope =
    Rope.foldr
        (\el acc ->
            if isGood el then
                el :: acc

            else
                acc
        )
        []
        rope
        |> Rope.Leaf


foldlToEndToFlat : (a -> Bool) -> Rope a -> Rope a
foldlToEndToFlat isGood rope =
    Rope.foldr
        (\el acc ->
            if isGood el then
                \end -> acc (el :: end)

            else
                acc
        )
        identity
        rope
        []
        |> Rope.Leaf


foldlReverseToFlat : (a -> Bool) -> Rope a -> Rope a
foldlReverseToFlat isGood rope =
    Rope.foldr
        (\el acc ->
            if isGood el then
                el :: acc

            else
                acc
        )
        []
        rope
        |> List.reverse
        |> Rope.Leaf


keepNested : (a -> Bool) -> Rope a -> Rope a
keepNested isGood rope =
    case rope of
        Rope.Leaf list ->
            Rope.Leaf (List.filter isGood list)

        Rope.Node ropes ->
            Rope.Node
                (List.foldr
                    (\e acc ->
                        let
                            filtered : Rope a
                            filtered =
                                keepNested isGood e
                        in
                        if Rope.isEmpty filtered then
                            acc

                        else
                            filtered :: acc
                    )
                    []
                    ropes
                )
