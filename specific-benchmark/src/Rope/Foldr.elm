module Rope.Foldr exposing (custom, customWithModuleLevelHelper, nested)

import Rope.Local as Rope exposing (Rope)


nested : (a -> b -> b) -> b -> Rope a -> b
nested f initialAcc rope =
    case rope of
        Rope.Leaf list ->
            List.foldr f initialAcc list

        Rope.Node ropes ->
            List.foldr (\childRope childAcc -> nested f childAcc childRope) initialAcc ropes


customWithModuleLevelHelper : (a -> b -> b) -> b -> Rope a -> b
customWithModuleLevelHelper f initialAcc rope =
    case rope of
        Rope.Leaf list ->
            List.foldr f initialAcc list

        Rope.Node ropes ->
            foldrHelperModuleLocal f [ ropes ] initialAcc


foldrHelperModuleLocal : (a -> (b -> b)) -> List (List (Rope a)) -> b -> b
foldrHelperModuleLocal f queue initialAcc =
    case queue of
        [] ->
            initialAcc

        [] :: tail ->
            foldrHelperModuleLocal f tail initialAcc

        (headHead :: headTail) :: tail ->
            case headHead of
                Rope.Leaf list ->
                    List.foldr f (foldrHelperModuleLocal f (headTail :: tail) initialAcc) list

                Rope.Node [] ->
                    foldrHelperModuleLocal f (headTail :: tail) initialAcc

                Rope.Node childRopes ->
                    foldrHelperModuleLocal f (childRopes :: headTail :: tail) initialAcc


custom : (a -> b -> b) -> b -> Rope a -> b
custom f initialAcc rope =
    case rope of
        Rope.Leaf list ->
            List.foldr f initialAcc list

        Rope.Node ropes ->
            let
                foldrHelper : List (List (Rope a)) -> b -> b
                foldrHelper queue res =
                    case queue of
                        [] ->
                            res

                        [] :: tail ->
                            foldrHelper tail res

                        (headHead :: headTail) :: tail ->
                            case headHead of
                                Rope.Leaf list ->
                                    List.foldr f (foldrHelper (headTail :: tail) res) list

                                Rope.Node [] ->
                                    foldrHelper (headTail :: tail) res

                                Rope.Node childRopes ->
                                    foldrHelper (childRopes :: headTail :: tail) res
            in
            foldrHelper [ ropes ] initialAcc
