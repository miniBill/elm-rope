module Rope.IndexedMap exposing (withFoldlReverse, withFoldlToEnd)

import Rope exposing (Rope)


withFoldlReverse : (Int -> a -> b) -> Rope a -> Rope b
withFoldlReverse f rope =
    rope
        |> Rope.foldl (\e ( i, acc ) -> ( i + 1, f i e :: acc )) ( 0, [] )
        |> Tuple.second
        |> List.reverse
        |> Rope.fromList


withFoldlToEnd : (Int -> a -> b) -> Rope a -> Rope b
withFoldlToEnd f rope =
    (rope
        |> Rope.foldl
            (\e ( i, acc ) ->
                ( i + 1
                , \end -> acc (f i e :: end)
                )
            )
            ( 0, identity )
        |> Tuple.second
    )
        []
        |> Rope.fromList
