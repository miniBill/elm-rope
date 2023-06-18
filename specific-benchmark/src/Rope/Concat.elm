module Rope.Concat exposing (nested, withAppend, withConsFoldToNode)

import Rope exposing (Rope)


withAppend : Rope (Rope a) -> Rope a
withAppend rope =
    case rope of
        Rope.Leaf list ->
            Rope.Node list

        Rope.Node _ ->
            Rope.foldl (\el acc -> Rope.appendTo acc el) Rope.empty rope


withConsFoldToNode : Rope (Rope a) -> Rope a
withConsFoldToNode rope =
    case rope of
        Rope.Leaf list ->
            Rope.Node list

        Rope.Node ropes ->
            Rope.Node
                (List.foldr
                    (\subRope acc ->
                        Rope.foldr (\el subAcc -> el :: subAcc) acc subRope
                    )
                    []
                    ropes
                )


nested : Rope (Rope a) -> Rope a
nested rope =
    case rope of
        Rope.Leaf list ->
            Rope.Node list

        Rope.Node ropes ->
            Rope.Node (List.map nested ropes)
