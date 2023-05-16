module Rope exposing
    ( Rope
    , empty, singleton, cons, snoc, fromList
    , map, indexedMap, foldl, foldr, filter, filterMap, toList
    , length, reverse, member, all, any, maximum, minimum, sum, product
    , append, concat, concatMap
    , isEmpty
    )

{-| A `Rope` is similar to a list, but has fast (constant time) concatenation at both ends, and fast concatenation of two `Rope`s.

It's slightly slower (O(n + operations) instead of O(n)) to iterate through, so you should convert it to a `List` if you plan to use it repeatedly.

Internally the `Rope` is a tree of lists.


# Types

@docs Rope


# Create

@docs empty, singleton, cons, snoc, fromList


# Transform

@docs map, indexedMap, foldl, foldr, filter, filterMap, toList


# Utilities

@docs length, reverse, member, all, any, maximum, minimum, sum, product


# Combine

@docs append, concat, concatMap


# Deconstruct

@docs isEmpty

-}


{-| -}
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

    cons 1 (fromList [2,3]) |> toList
    --> [1,2,3]
    cons 1 empty |> toList
    --> [1]

This operator is pronounced _cons_ for historical reasons, but you can think
of it like pushing an entry onto a stack.

Complexity: O(1)

-}
cons : a -> Rope a -> Rope a
cons head tail =
    Node [ Leaf [ head ], tail ]


{-| Add an element to the end of a list.

    snoc 1 (fromList [2,3]) |> toList
    --> [2,3,1]
    snoc 1 empty |> toList
    --> [1]

This operator is pronounced _snoc_ for historical reasons, but you can think
of it like pushing an entry at the end of a queue.

Complexity: O(1)

-}
snoc : a -> Rope a -> Rope a
snoc tail head =
    Node [ head, Leaf [ tail ] ]


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
    foldr (\x acc -> cons (f x) acc) empty rope


{-| Same as `map` but the function is also applied to the index of each
element (starting at zero).

    indexedMap Tuple.pair (fromList [ "Tom", "Sue", "Bob" ]) |> toList
    --> [ ( 0, "Tom" ), ( 1, "Sue" ), ( 2, "Bob" ) ]

Complexity: O(n)

-}
indexedMap : (Int -> a -> b) -> Rope a -> Rope b
indexedMap f xs =
    xs
        |> foldl (\e ( i, acc ) -> ( i + 1, f i e :: acc )) ( 0, [] )
        |> Tuple.second
        |> List.reverse
        |> fromList


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
foldl f acc rope =
    case rope of
        Leaf list ->
            List.foldl f acc list

        Node ropes ->
            List.foldl (\childRope childAcc -> foldl f childAcc childRope) acc ropes


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
foldr f acc rope =
    case rope of
        Leaf list ->
            List.foldr f acc list

        Node ropes ->
            List.foldr (\childRope childAcc -> foldr f childAcc childRope) acc ropes


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
                    (\x acc ->
                        let
                            filtered : Rope a
                            filtered =
                                filter isGood x
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
filterMap isGood rope =
    case rope of
        Leaf list ->
            Leaf (List.filterMap isGood list)

        Node ropes ->
            Node
                (List.foldr
                    (\x acc ->
                        let
                            filtered : Rope b
                            filtered =
                                filterMap isGood x
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
    foldl (::) [] rope |> List.reverse



-- UTILITIES


{-| Determine the length of a rope.

    length (fromList [1,2,3])
    --> 3

-}
length : Rope a -> Int
length xs =
    foldl (\_ i -> i + 1) 0 xs


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
member x xs =
    any (\a -> a == x) xs


{-| Determine if all elements satisfy some test.

    all (\n -> modBy 2 n == 0) (fromList [2,4])
    --> True
    all (\n -> modBy 2 n == 0) (fromList [2,3])
    --> False
    all (\n -> modBy 2 n == 0) (fromList [])
    --> True

-}
all : (a -> Bool) -> Rope a -> Bool
all isOkay list =
    not (any (\a -> not (isOkay a)) list)


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
            List.any (any isOkay) ropes


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

        Node ropes ->
            ropes
                |> List.filterMap maximum
                |> List.maximum


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

        Node ropes ->
            ropes
                |> List.filterMap minimum
                |> List.minimum


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
    foldl (+) 0 numbers


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
    foldl (*) 1 numbers



-- COMBINE


{-| Put two ropes together.

    append (fromList [ 1, 1, 2 ]) (fromList [ 3, 5, 8 ]) |> toList
    --> [ 1, 1, 2, 3, 5, 8 ]

    append (fromList [ 'a', 'b' ]) (fromList [ 'c' ]) |> toList
    --> [ 'a', 'b', 'c' ]

Complexity: O(1)

-}
append : Rope a -> Rope a -> Rope a
append left right =
    Node [ left, right ]


{-| Concatenate a bunch of ropes into a single rope:

    concat [ fromList [ 1, 2 ], fromList [ 3 ], fromList [ 4, 5 ] ] |> toList
    --> [ 1, 2, 3, 4, 5 ]

Complexity: O(1)

-}
concat : Rope (Rope a) -> Rope a
concat ropes =
    -- the inversion is intentional!
    foldl (\rope acc -> append acc rope) empty ropes


{-| Map a given function onto a list and flatten the resulting lists.

    concatMap f xs == concat (map f xs)

-}
concatMap : (a -> Rope b) -> Rope a -> Rope b
concatMap f ropes =
    -- the inversion is intentional!
    foldl (\rope acc -> append acc (f rope)) empty ropes



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
