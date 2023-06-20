module Rope exposing
    ( Rope
    , empty, singleton, append, prepend, fromList
    , map, indexedMap, foldl, foldr, filter, filterMap, toList
    , length, reverse, member, all, any, maximum, minimum, sum, product
    , appendTo, prependTo, concat, concatMap
    , isEmpty
    )

{-|


# Types

@docs Rope


# Create

@docs empty, singleton, append, prepend, fromList


# Transform

@docs map, indexedMap, foldl, foldr, filter, filterMap, toList


# Utilities

@docs length, reverse, member, all, any, maximum, minimum, sum, product


# Combine

@docs appendTo, prependTo, concat, concatMap


# Deconstruct

@docs isEmpty

-}

import Util exposing (listAll, listFilledFoldl1Map, listProductMap, listSumMap)


{-| A `Rope` is similar to a list, but has fast (constant time) concatenation at both ends, and fast concatenation of two `Rope`s.

It's slightly slower (O(n + operations) instead of O(n)) to iterate through, so you should convert it to a `List` if you plan to use it repeatedly.

Internally the `Rope` is a tree of lists.

-}
type Rope a
    = Leaf (List a)
    | Node (List (Rope a))



-- CREATE


{-| An empty rope.
-}
empty : Rope a
empty =
    Leaf []


{-| Create a rope with only one element:

    singleton 1234 |> toList
    --> [ 1234 ]

    singleton "hi" |> toList
    --> [ "hi" ]

-}
singleton : a -> Rope a
singleton value =
    Leaf [ value ]


{-| Add an element to the front of a list.

    prepend 1 (fromList [2,3]) |> toList
    --> [1,2,3]
    prepend 1 empty |> toList
    --> [1]

Complexity: O(1)

-}
prepend : a -> Rope a -> Rope a
prepend head tail =
    case tail of
        Leaf list ->
            Leaf (head :: list)

        Node ropes ->
            Node (Leaf [ head ] :: ropes)


{-| Add an element to the end of a list.

    append 1 (fromList [2,3]) |> toList
    --> [2,3,1]
    append 1 empty |> toList
    --> [1]

Complexity: O(1)

-}
append : a -> Rope a -> Rope a
append last init =
    Node [ init, Leaf [ last ] ]


{-| Build a rope from a list.

Complexity: O(1)

-}
fromList : List a -> Rope a
fromList list =
    Leaf list



-- TRANSFORM


{-| Apply a function to every element of a rope.

    map sqrt (fromList [1,4,9]) |> toList
    --> [1,2,3]

    map not (fromList [True,False,True]) |> toList
    --> [False,True,False]

So `map func (fromList [ a, b, c ])` is the same as `fromList [ func a, func b, func c ]`

Complexity: O(n)

-}
map : (a -> b) -> Rope a -> Rope b
map f rope =
    rope
        |> foldl (\e acc -> f e :: acc) []
        |> List.reverse
        |> Leaf


{-| Same as `map` but the function is also applied to the index of each
element (starting at zero).

    indexedMap Tuple.pair (fromList [ "Tom", "Sue", "Bob" ]) |> toList
    --> [ ( 0, "Tom" ), ( 1, "Sue" ), ( 2, "Bob" ) ]

Complexity: O(n)

-}
indexedMap : (Int -> a -> b) -> Rope a -> Rope b
indexedMap f rope =
    rope
        |> foldl (\e ( i, acc ) -> ( i + 1, f i e :: acc )) ( 0, [] )
        |> Tuple.second
        |> List.reverse
        |> Leaf


{-| Reduce a rope from the left.

    foldl (+) 0 (fromList [ 1, 2, 3 ])
    --> 6

    foldl (::) [] (fromList [ 1, 2, 3 ])
    --> [ 3, 2, 1 ]

So `foldl step state [1,2,3]` is like saying:

    state
        |> step 1
        |> step 2
        |> step 3

Complexity: O(n)

-}
foldl : (a -> b -> b) -> b -> Rope a -> b
foldl f initialAcc rope =
    case rope of
        Leaf list ->
            List.foldl f initialAcc list

        Node [] ->
            initialAcc

        Node ropes ->
            let
                foldlHelper : List (List (Rope a)) -> b -> b
                foldlHelper queue res =
                    case queue of
                        [] ->
                            res

                        [] :: tail ->
                            foldlHelper tail res

                        ((Leaf list) :: headTail) :: tail ->
                            foldlHelper (headTail :: tail) (List.foldl f res list)

                        ((Node []) :: headTail) :: tail ->
                            foldlHelper (headTail :: tail) res

                        ((Node childRopes) :: headTail) :: tail ->
                            foldlHelper (childRopes :: headTail :: tail) res
            in
            foldlHelper [ ropes ] initialAcc


{-| Reduce a rope from the right.

    foldr (+) 0 (fromList [ 1, 2, 3 ])
    --> 6

    foldr (::) [] (fromList [ 1, 2, 3 ])
    --> [ 1, 2, 3 ]

So `foldr step state [1,2,3]` is like saying:

    state
        |> step 3
        |> step 2
        |> step 1

-}
foldr : (a -> b -> b) -> b -> Rope a -> b
foldr f initialAcc rope =
    case rope of
        Leaf list ->
            List.foldr f initialAcc list

        Node ropes ->
            List.foldr (\childRope childAcc -> foldr f childAcc childRope) initialAcc ropes


{-| Keep elements that satisfy the test.

    filter (\n -> modBy 2 n == 0) (fromList [1,2,3,4,5,6]) |> toList
    --> [2,4,6]

-}
filter : (a -> Bool) -> Rope a -> Rope a
filter isGood rope =
    case rope of
        Leaf list ->
            Leaf (List.filter isGood list)

        Node ropes ->
            Node
                (List.foldr
                    (\e acc ->
                        let
                            filtered : Rope a
                            filtered =
                                filter isGood e
                        in
                        if isEmpty filtered then
                            acc

                        else
                            filtered :: acc
                    )
                    []
                    ropes
                )


{-| Filter out certain values. For example, maybe you have a bunch of strings
from an untrusted source and you want to turn them into numbers:


    numbers : List Int
    numbers =
        filterMap String.toInt [ "3", "hi", "12", "4th", "May" ]

    -- numbers == [3, 12]

-}
filterMap : (a -> Maybe b) -> Rope a -> Rope b
filterMap f rope =
    case rope of
        Leaf list ->
            Leaf (List.filterMap f list)

        Node ropes ->
            Node
                (List.foldr
                    (\e acc ->
                        let
                            filtered : Rope b
                            filtered =
                                filterMap f e
                        in
                        if isEmpty filtered then
                            acc

                        else
                            filtered :: acc
                    )
                    []
                    ropes
                )


{-| Convert a rope into the equivalent list.

Complexity: O(n)

-}
toList : Rope a -> List a
toList rope =
    case rope of
        Leaf list ->
            list

        Node _ ->
            foldr (::) [] rope



-- UTILITIES


{-| Determine the length of a rope.

    length (fromList [1,2,3])
    --> 3

-}
length : Rope a -> Int
length rope =
    case rope of
        Leaf list ->
            List.length list

        Node ropes ->
            listSumMap length ropes


{-| Reverse a rope.

    reverse [ 1, 2, 3, 4 ] == [ 4, 3, 2, 1 ]

-}
reverse : Rope a -> Rope a
reverse rope =
    Leaf <| foldl (::) [] rope


{-| Figure out whether a rope contains a value.

    member 9 (fromList [1,2,3,4])
    --> False
    member 4 (fromList [1,2,3,4])
    --> True

-}
member : a -> Rope a -> Bool
member needle rope =
    any (\a -> a == needle) rope


{-| Determine if all elements satisfy some test.

    all (\n -> modBy 2 n == 0) (fromList [2,4])
    --> True
    all (\n -> modBy 2 n == 0) (fromList [2,3])
    --> False
    all (\n -> modBy 2 n == 0) (fromList [])
    --> True

-}
all : (a -> Bool) -> Rope a -> Bool
all isOkay rope =
    case rope of
        Leaf list ->
            listAll isOkay list

        Node ropes ->
            listAll (\subRope -> all isOkay subRope) ropes


{-| Determine if any elements satisfy some test.

    any (\n -> modBy 2 n == 0) (fromList [ 2, 3 ])
    --> True

    any (\n -> modBy 2 n == 0) (fromList [ 1, 3 ])
    --> False

    any (\n -> modBy 2 n == 0) (fromList [])
    --> False

-}
any : (a -> Bool) -> Rope a -> Bool
any isOkay rope =
    case rope of
        Leaf list ->
            List.any isOkay list

        Node ropes ->
            List.any (\subRope -> any isOkay subRope) ropes


{-| Find the maximum element in a non-empty rope.

    maximum (fromList [ 1, 4, 2 ])
    --> Just 4

    maximum (fromList [])
    --> Nothing

-}
maximum : Rope comparable -> Maybe comparable
maximum rope =
    case rope of
        Leaf list ->
            List.maximum list

        Node [] ->
            Nothing

        Node (headSubRope :: tailSubRopes) ->
            listFilledFoldl1Map maximum
                (\a b ->
                    case a of
                        Nothing ->
                            b

                        Just aContent ->
                            case b of
                                Nothing ->
                                    Just aContent

                                Just bContent ->
                                    Just (Basics.max aContent bContent)
                )
                ( headSubRope, tailSubRopes )


{-| Find the minimum element in a non-empty rope.

    minimum (fromList [ 3, 2, 1 ])
    --> Just 1

    minimum (fromList [])
    --> Nothing

-}
minimum : Rope comparable -> Maybe comparable
minimum rope =
    case rope of
        Leaf list ->
            List.minimum list

        Node [] ->
            Nothing

        Node (headSubRope :: tailSubRopes) ->
            listFilledFoldl1Map maximum
                (\a b ->
                    case a of
                        Nothing ->
                            b

                        Just aContent ->
                            case b of
                                Nothing ->
                                    Just aContent

                                Just bContent ->
                                    Just (Basics.min aContent bContent)
                )
                ( headSubRope, tailSubRopes )


{-| Get the sum of the rope elements.

    sum (fromList [ 1, 2, 3 ])
    --> 6

    sum (fromList [ 1, 1, 1 ])
    --> 3

    sum (fromList [])
    --> 0

-}
sum : Rope number -> number
sum numbers =
    case numbers of
        Leaf list ->
            List.sum list

        Node ropes ->
            listSumMap sum ropes


{-| Get the product of the rope elements.

    product (fromList [ 2, 2, 2 ])
    --> 8

    product (fromList [ 3, 3, 3 ])
    --> 27

    product (fromList [])
    --> 1

-}
product : Rope number -> number
product numbers =
    case numbers of
        Leaf list ->
            List.product list

        Node ropes ->
            listProductMap product ropes



-- COMBINE


{-| Put two ropes together, the second after the first.

    appendTo (fromList [ 1, 1, 2 ]) (fromList [ 3, 5, 8 ]) |> toList
    --> [ 1, 1, 2, 3, 5, 8 ]

    appendTo (fromList [ 'a', 'b' ]) (fromList [ 'c' ]) |> toList
    --> [ 'a', 'b', 'c' ]

Complexity: O(1)

-}
appendTo : Rope a -> Rope a -> Rope a
appendTo early late =
    case late of
        Node ropes ->
            Node (early :: ropes)

        Leaf _ ->
            Node [ early, late ]


{-| Put two ropes together, the first after the second.

    prependTo (fromList [ 1, 1, 2 ]) (fromList [ 3, 5, 8 ]) |> toList
    --> [ 3, 5, 8, 1, 1, 2 ]

    prependTo (fromList [ 'a', 'b' ]) (fromList [ 'c' ]) |> toList
    --> [ 'c', 'a', 'b' ]

Complexity: O(1)

-}
prependTo : Rope a -> Rope a -> Rope a
prependTo late early =
    case late of
        Node ropes ->
            Node (early :: ropes)

        Leaf _ ->
            Node [ early, late ]


{-| Concatenate a bunch of ropes into a single rope:

    concat (fromList [ fromList [ 1, 2 ], fromList [ 3 ], fromList [ 4, 5 ] ]) |> toList
    --> [ 1, 2, 3, 4, 5 ]

Complexity: O(n), in practice it can be O(1) if the top level is the result of `fromList`

-}
concat : Rope (Rope a) -> Rope a
concat ropes =
    case ropes of
        Leaf list ->
            Node list

        Node children ->
            Node (List.map concat children)


{-| Map a given function onto a list and flatten the resulting lists.

    concatMap f xs == concat (map f xs)

-}
concatMap : (a -> Rope b) -> Rope a -> Rope b
concatMap f rope =
    case rope of
        Leaf list ->
            Node (List.map f list)

        Node ropes ->
            Node
                (List.foldr
                    (\subRope acc ->
                        foldr (\el subAcc -> f el :: subAcc) acc subRope
                    )
                    []
                    ropes
                )



-- DECONSTRUCT


{-| Determine if a rope is empty.

    isEmpty (fromList [])
    --> True

-}
isEmpty : Rope a -> Bool
isEmpty rope =
    case rope of
        Leaf list ->
            List.isEmpty list

        Node ropes ->
            List.all isEmpty ropes
