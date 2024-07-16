module Rope.Map exposing (foldlReverseToFlat, foldrToFlat, keepNested)

import Rope.Local as Rope exposing (Rope)


foldlReverseToFlat : (a -> b) -> Rope a -> Rope b
foldlReverseToFlat f rope =
    rope
        |> Rope.foldl (\e acc -> f e :: acc) []
        |> List.reverse
        |> Rope.fromList


foldrToFlat : (a -> b) -> Rope a -> Rope b
foldrToFlat f rope =
    rope
        |> Rope.foldr (\e acc -> f e :: acc) []
        |> Rope.fromList


keepNested : (a -> b) -> Rope a -> Rope b
keepNested f rope =
    case rope of
        Rope.Leaf list ->
            Rope.fromList (List.map f list)

        Rope.Node ropes ->
            Rope.Node (List.map (\subRope -> keepNested f subRope) ropes)
