module Rope.All exposing (nested, notAnyNot)

import Rope exposing (Rope)
import Util exposing (listAll)


notAnyNot : (a -> Bool) -> Rope a -> Bool
notAnyNot isOkay rope =
    not (Rope.any (\a -> not (isOkay a)) rope)


nested : (a -> Bool) -> Rope a -> Bool
nested isOkay rope =
    case rope of
        Rope.Leaf list ->
            listAll isOkay list

        Rope.Node ropes ->
            listAll (nested isOkay) ropes
