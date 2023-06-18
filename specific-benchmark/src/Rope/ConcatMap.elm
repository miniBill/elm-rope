module Rope.ConcatMap exposing (nested, withAppend, withConsFoldToNode)

import Rope exposing (Rope)


withAppend : (a -> Rope b) -> Rope a -> Rope b
withAppend f rope =
    case rope of
        Rope.Leaf list ->
            Rope.Node (List.map f list)

        Rope.Node _ ->
            Rope.foldl (\el acc -> Rope.appendTo acc (f el)) Rope.empty rope


withConsFoldToNode : (a -> Rope b) -> Rope a -> Rope b
withConsFoldToNode f rope =
    case rope of
        Rope.Leaf list ->
            Rope.Node (List.map f list)

        Rope.Node ropes ->
            Rope.Node
                (List.foldr
                    (\subRope acc ->
                        Rope.foldr (\el subAcc -> f el :: subAcc) acc subRope
                    )
                    []
                    ropes
                )


nested : (a -> Rope b) -> Rope a -> Rope b
nested f rope =
    case rope of
        Rope.Leaf list ->
            Rope.Node (List.map f list)

        Rope.Node ropes ->
            Rope.Node (List.map (\subRope -> nested f subRope) ropes)
