module Rope.Reverse exposing (foldlToFlat, foldrToEndToFlat, keepNested)

import Rope.Local as Rope exposing (Rope)
import Util exposing (listReverseMap)


foldlToFlat : Rope a -> Rope a
foldlToFlat rope =
    Rope.Leaf <| Rope.foldl (::) [] rope


foldrToEndToFlat : Rope a -> Rope a
foldrToEndToFlat rope =
    Rope.foldr
        (\new soFar ->
            \end -> soFar (new :: end)
        )
        identity
        rope
        []
        |> Rope.Leaf


keepNested : Rope a -> Rope a
keepNested rope =
    case rope of
        Rope.Leaf list ->
            Rope.Leaf (List.reverse list)

        Rope.Node ropes ->
            Rope.Node (listReverseMap keepNested ropes)
