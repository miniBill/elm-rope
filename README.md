# elm-rope

A `Rope` is a data structure similar to a list, but supporting fast concatenation at both ends.

It's useful when you're concatenating many lists, especially if recursively. You can build a `Rope` and then call `toList` at the end and only do the concatenation once.

# Example

```elm
import Rope

ten : Rope Int
ten =
    Rope.fromList (List.range 1 10)

result : List Int
result =
    ten
        |> Rope.concatMap
            (\one ->
                ten
                    |> Rope.concatMap
                        (\two ->
                            ten
                                |> Rope.concatMap
                                    (\three ->
                                        Rope.fromList [ one, two, three ]
                                    )
                        )
            )
        |> Rope.toList
```
