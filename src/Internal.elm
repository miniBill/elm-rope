module Internal exposing (Rope(..))


type Rope a
    = Leaf (List a)
    | Node (List (Rope a))
