module Rope.FilterMap exposing (foldlReverseToFlat, foldlToEndToFlat, foldrToFlat, keepNested)

import Rope.Local as Rope exposing (Rope)


foldrToFlat : (a -> Maybe b) -> Rope a -> Rope b
foldrToFlat try rope =
    Rope.foldr
        (\el acc ->
            case try el of
                Just elSuccess ->
                    elSuccess :: acc

                Nothing ->
                    acc
        )
        []
        rope
        |> Rope.Leaf


foldlToEndToFlat : (a -> Maybe b) -> Rope a -> Rope b
foldlToEndToFlat try rope =
    Rope.foldr
        (\el acc ->
            case try el of
                Just elSuccess ->
                    \end -> acc (elSuccess :: end)

                Nothing ->
                    acc
        )
        identity
        rope
        []
        |> Rope.Leaf


foldlReverseToFlat : (a -> Maybe b) -> Rope a -> Rope b
foldlReverseToFlat try rope =
    Rope.foldr
        (\el acc ->
            case try el of
                Just elSuccess ->
                    elSuccess :: acc

                Nothing ->
                    acc
        )
        []
        rope
        |> List.reverse
        |> Rope.Leaf


keepNested : (a -> Maybe b) -> Rope a -> Rope b
keepNested try rope =
    case rope of
        Rope.Leaf list ->
            Rope.Leaf (List.filterMap try list)

        Rope.Node ropes ->
            Rope.Node
                (List.foldr
                    (\e acc ->
                        let
                            filtered : Rope b
                            filtered =
                                keepNested try e
                        in
                        if (filtered == Rope.Leaf []) || (filtered == Rope.Node []) then
                            acc

                        else
                            filtered :: acc
                    )
                    []
                    ropes
                )
