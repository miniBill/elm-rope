module Suite exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Rope exposing (Rope)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "elm-rope"
        [ Test.fuzz (Fuzz.list Fuzz.int) "fromList |> toList doesn't change the list" <|
            \list ->
                list
                    |> Rope.fromList
                    |> Rope.toList
                    |> Expect.equalLists list
        , Test.fuzz (Fuzz.pair (Fuzz.list Fuzz.int) (Fuzz.list Fuzz.int))
            "|> prependTo is like ++"
          <|
            \( aList, bList ) ->
                aList
                    |> Rope.fromList
                    |> Rope.prependTo (bList |> Rope.fromList)
                    |> Rope.toList
                    |> Expect.equalLists (aList ++ bList)
        , Test.fuzz (Fuzz.pair (Fuzz.list Fuzz.int) (Fuzz.list Fuzz.int))
            "a |> appendTo b is like b ++ a"
          <|
            \( aList, bList ) ->
                aList
                    |> Rope.fromList
                    |> Rope.appendTo (bList |> Rope.fromList)
                    |> Rope.toList
                    |> Expect.equalLists (bList ++ aList)
        , Test.fuzz (Fuzz.pair Fuzz.int (Fuzz.list Fuzz.int))
            "prepend is like ::"
          <|
            \( head, tail ) ->
                tail
                    |> Rope.fromList
                    |> Rope.prepend head
                    |> Rope.toList
                    |> Expect.equalLists (head :: tail)
        , Test.fuzz (Fuzz.pair (Fuzz.list Fuzz.int) Fuzz.int)
            "append is like ++ [ _ ]"
          <|
            \( init, last ) ->
                init
                    |> Rope.fromList
                    |> Rope.append last
                    |> Rope.toList
                    |> Expect.equalLists (init ++ [ last ])
        , Test.fuzz (ropeFuzz (ropeFuzz Fuzz.int))
            "concat behaves like List.concat"
          <|
            \rope ->
                rope
                    |> Rope.concat
                    |> Rope.toList
                    |> Expect.equalLists
                        (rope
                            |> Rope.toList
                            |> List.concatMap Rope.toList
                        )
        , Test.fuzz (ropeFuzz (ropeFuzz Fuzz.int))
            "concatMap identity behaves like concat"
          <|
            \rope ->
                rope
                    |> Rope.concatMap identity
                    |> Rope.toList
                    |> Expect.equal
                        (rope
                            |> Rope.concat
                            |> Rope.toList
                        )
        , Test.fuzz (ropeFuzz Fuzz.int) "map doesn't change its length" <|
            \rope ->
                rope
                    |> Rope.map negate
                    |> Rope.length
                    |> Expect.equal
                        (rope |> Rope.length)
        , Test.fuzz (ropeFuzz Fuzz.int) "map behaves like List.map" <|
            \rope ->
                rope
                    |> Rope.map negate
                    |> Rope.toList
                    |> Expect.equalLists
                        (rope
                            |> Rope.toList
                            |> List.map negate
                        )
        , Test.fuzz (ropeFuzz Fuzz.int) "indexedMap behaves like List.indexedMap" <|
            \rope ->
                rope
                    |> Rope.indexedMap Tuple.pair
                    |> Rope.toList
                    |> Expect.equalLists
                        (rope
                            |> Rope.toList
                            |> List.indexedMap Tuple.pair
                        )
        , Test.fuzz (ropeFuzz Fuzz.int) "filter behaves like List.filter" <|
            \rope ->
                rope
                    |> Rope.filter (\n -> remainderBy 2 n == 0)
                    |> Rope.toList
                    |> Expect.equalLists
                        (rope
                            |> Rope.toList
                            |> List.filter (\n -> remainderBy 2 n == 0)
                        )
        , Test.fuzz (ropeFuzz Fuzz.int) "foldl behaves like List.foldl" <|
            \rope ->
                rope
                    |> Rope.foldl (-) 10
                    |> Expect.equal
                        (rope
                            |> Rope.toList
                            |> List.foldl (-) 10
                        )
        , Test.fuzz (ropeFuzz Fuzz.int) "foldr behaves like List.foldr" <|
            \rope ->
                rope
                    |> Rope.foldr (-) 10
                    |> Expect.equal
                        (rope
                            |> Rope.toList
                            |> List.foldr (-) 10
                        )
        , Test.fuzz (ropeFuzz Fuzz.int) "filterMap behaves like List.filterMap" <|
            \rope ->
                let
                    toEven : Int -> Maybe Int
                    toEven n =
                        if remainderBy 2 n == 0 then
                            Just n

                        else
                            Nothing
                in
                rope
                    |> Rope.filterMap toEven
                    |> Rope.toList
                    |> Expect.equalLists
                        (rope
                            |> Rope.toList
                            |> List.filterMap toEven
                        )
        , Test.fuzz (ropeFuzz Fuzz.int) "length behaves like List.length" <|
            \rope ->
                rope
                    |> Rope.length
                    |> Expect.equal
                        (rope
                            |> Rope.toList
                            |> List.length
                        )
        , Test.fuzz (ropeFuzz Fuzz.int) "reverse behaves like List.reverse" <|
            \rope ->
                rope
                    |> Rope.reverse
                    |> Rope.toList
                    |> Expect.equalLists
                        (rope
                            |> Rope.toList
                            |> List.reverse
                        )
        , Test.fuzz (ropeFuzz (Fuzz.intRange 0 20)) "member behaves like List.member" <|
            \rope ->
                rope
                    |> Rope.member 0
                    |> Expect.equal
                        (rope
                            |> Rope.toList
                            |> List.member 0
                        )
        , Test.fuzz (ropeFuzz (Fuzz.intRange 0 20)) "all behaves like List.all" <|
            \rope ->
                rope
                    |> Rope.all (\n -> n /= 0)
                    |> Expect.equal
                        (rope
                            |> Rope.toList
                            |> List.all (\n -> n /= 0)
                        )
        , Test.fuzz (ropeFuzz (Fuzz.intRange 0 20)) "any behaves like List.any" <|
            \rope ->
                rope
                    |> Rope.all (\n -> n == 0)
                    |> Expect.equal
                        (rope
                            |> Rope.toList
                            |> List.all (\n -> n == 0)
                        )
        , Test.fuzz (ropeFuzz Fuzz.int) "maximum behaves like List.maximum" <|
            \rope ->
                rope
                    |> Rope.maximum
                    |> Expect.equal
                        (rope
                            |> Rope.toList
                            |> List.maximum
                        )
        , Test.fuzz (ropeFuzz Fuzz.int) "minimum behaves like List.minimum" <|
            \rope ->
                rope
                    |> Rope.minimum
                    |> Expect.equal
                        (rope
                            |> Rope.toList
                            |> List.minimum
                        )
        , Test.fuzz (ropeFuzz Fuzz.int) "sum behaves like List.sum" <|
            \rope ->
                rope
                    |> Rope.sum
                    |> Expect.equal
                        (rope
                            |> Rope.toList
                            |> List.sum
                        )
        , Test.fuzz (ropeFuzz (Fuzz.floatRange -5 5)) "product behaves like List.product" <|
            \rope ->
                rope
                    |> Rope.product
                    |> Expect.within (Expect.Relative 0.001)
                        (rope
                            |> Rope.toList
                            |> List.product
                        )
        , Test.fuzz (ropeFuzz Fuzz.int) "isEmpty behaves like List.isEmpty" <|
            \rope ->
                rope
                    |> Rope.isEmpty
                    |> Expect.equal
                        (rope
                            |> Rope.toList
                            |> List.isEmpty
                        )
        ]


ropeFuzz : Fuzzer a -> Fuzzer (Rope a)
ropeFuzz elementFuzz =
    ropeFuzzAtDepth 0 elementFuzz


ropeFuzzAtDepth : Int -> Fuzzer a -> Fuzzer (Rope a)
ropeFuzzAtDepth depth elementFuzz =
    if depth <= 3 then
        Fuzz.map (\ropes -> ropes |> Rope.fromList |> Rope.concat)
            (Fuzz.listOfLengthBetween 0 4 (ropeFuzzAtDepth (depth + 1) elementFuzz))

    else
        -- depth >= 4
        Fuzz.map Rope.fromList
            (Fuzz.listOfLengthBetween 0 4 elementFuzz)
