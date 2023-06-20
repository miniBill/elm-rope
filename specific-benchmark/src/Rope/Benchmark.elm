module Rope.Benchmark exposing (all, exampleRopeOfRopes)

import Benchmark exposing (Benchmark)
import Benchmark.Alternative
import List.All
import Rope.All
import Rope.Concat
import Rope.ConcatMap
import Rope.IndexedMap
import Rope.Length
import Rope.Local as Rope exposing (Rope)
import Rope.Map
import Rope.Minimum
import Rope.Sum
import Rope.ToList


all : Benchmark
all =
    Benchmark.describe "for elm-rope"
        [ Benchmark.describe "List"
            [ Benchmark.Alternative.rank "all"
                (\candidate -> candidate (\a -> a /= 500) exampleList)
                [ ( "recursive", List.All.recursive )
                , ( "not (any (not ...))", List.All.notAnyNot )
                , ( "not (any (not << ...))", List.All.notAnyNotComposition )
                , ( "not (any isBad) with let isBad = not ...", List.All.notAnyNotFromLet )
                ]
            ]
        , Benchmark.describe "Rope"
            [ Benchmark.Alternative.rank "fold -l vs -r"
                (\fold -> fold (+) 0 exampleRope)
                [ ( "foldr", Rope.foldr )
                , ( "foldl", Rope.foldl )
                ]
            , Benchmark.Alternative.rank "toList"
                (\candidate -> candidate exampleRope)
                [ ( "with foldl |> reverse", Rope.ToList.withFoldlReverse )
                , ( "with foldr", Rope.ToList.withFoldr )
                , ( "with foldr preserving last list", Rope.ToList.withFoldrPreservingLastList )
                ]
            , Benchmark.Alternative.rank "length"
                (\candidate -> candidate exampleRope)
                [ ( "with foldl", Rope.Length.withFoldl )
                , ( "with foldr", Rope.Length.withFoldr )
                , ( "nested", Rope.Length.nested )
                ]
            , Benchmark.Alternative.rank "all"
                (\candidate -> candidate (\a -> a /= 19) exampleRope)
                [ ( "nested", Rope.All.nested )
                , ( "not (any (not << ...))", Rope.All.notAnyNot )
                ]
            , Benchmark.Alternative.rank "sum (also applies to product)"
                (\candidate -> candidate exampleRope)
                [ ( "with foldr", Rope.Sum.withFoldr )
                , ( "with foldl", Rope.Sum.withFoldl )
                , ( "nested", Rope.Sum.nested )
                ]
            , Benchmark.Alternative.rank "minimum (also applies to maximum)"
                (\candidate -> candidate exampleRope)
                [ ( "with filterMap", Rope.Minimum.withFilterMap )
                , ( "with foldr", Rope.Minimum.withFoldr )
                , ( "with foldl", Rope.Minimum.withFoldl )
                , ( "with nested fold", Rope.Minimum.withNestedFold )
                ]
            , Benchmark.Alternative.rank "map"
                (\candidate -> candidate (\n -> n + 1) exampleRope)
                [ ( "to flat with foldl |> reverse", Rope.Map.foldlReverseToFlat )
                , ( "to flat with foldr", Rope.Map.foldrToFlat )
                , ( "keep nested", Rope.Map.keepNested )
                ]
            , Benchmark.Alternative.rank "indexedMap"
                (\candidate -> candidate (\i n -> n + i) exampleRope)
                [ ( "with foldl |> reverse", Rope.IndexedMap.withFoldlReverse )
                , ( "with foldl to end", Rope.IndexedMap.withFoldlToEnd )
                ]
            , Benchmark.Alternative.rank "concat"
                (\candidate -> candidate exampleRopeOfRopes)
                [ ( "with append", Rope.Concat.withAppend )
                , ( "nested", Rope.Concat.nested )
                , ( "with :: fold to Node", Rope.Concat.withConsFoldToNode )
                ]
            , Benchmark.Alternative.rank "concatMap"
                (\candidate -> candidate Rope.singleton exampleRope)
                [ ( "with append", Rope.ConcatMap.withAppend )
                , ( "nested", Rope.ConcatMap.nested )
                , ( "with :: fold to Node", Rope.ConcatMap.withConsFoldToNode )
                ]
            ]
        ]


exampleList : List Int
exampleList =
    List.range 0 1000


exampleRopeOfRopes : Rope (Rope Int)
exampleRopeOfRopes =
    Rope.fromList (List.range 0 12)
        |> Rope.concatMap
            (\levelOne ->
                Rope.fromList (List.range 13 19)
                    |> Rope.concatMap
                        (\levelTwo ->
                            Rope.fromList (List.range 20 24)
                                |> Rope.map
                                    (\levelThree ->
                                        Rope.fromList (List.range 25 26)
                                            |> Rope.concatMap
                                                (\levelFour ->
                                                    Rope.fromList [ levelOne, levelTwo, levelThree, levelFour ]
                                                )
                                    )
                        )
            )


exampleRope : Rope Int
exampleRope =
    Rope.fromList (List.range 0 14)
        |> Rope.concatMap
            (\levelOne ->
                Rope.fromList (List.range 15 24)
                    |> Rope.concatMap
                        (\levelTwo ->
                            Rope.fromList (List.range 25 29)
                                |> Rope.concatMap
                                    (\levelThree ->
                                        Rope.fromList [ levelOne, levelTwo, levelThree ]
                                    )
                        )
            )
