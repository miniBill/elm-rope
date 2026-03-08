module Rope.Foldl exposing (custom, customWithModuleLevelHelper, nested)

import Rope.Local as Rope exposing (Rope)


custom : (a -> b -> b) -> b -> Rope a -> b
custom f initialAcc rope =
    case rope of
        Rope.Leaf list ->
            List.foldl f initialAcc list

        Rope.Node [] ->
            initialAcc

        Rope.Node ropes ->
            let
                foldlHelper : List (List (Rope a)) -> b -> b
                foldlHelper queue res =
                    case queue of
                        [] ->
                            res

                        [] :: tail ->
                            foldlHelper tail res

                        ((Rope.Leaf list) :: headTail) :: tail ->
                            foldlHelper (headTail :: tail) (List.foldl f res list)

                        ((Rope.Node []) :: headTail) :: tail ->
                            foldlHelper (headTail :: tail) res

                        ((Rope.Node childRopes) :: headTail) :: tail ->
                            foldlHelper (childRopes :: headTail :: tail) res
            in
            foldlHelper [ ropes ] initialAcc


customWithModuleLevelHelper : (a -> b -> b) -> b -> Rope a -> b
customWithModuleLevelHelper f initialAcc rope =
    case rope of
        Rope.Leaf list ->
            List.foldl f initialAcc list

        Rope.Node [] ->
            initialAcc

        Rope.Node ropes ->
            foldlHelperModuleLocal f [ ropes ] initialAcc


foldlHelperModuleLocal : (a -> b -> b) -> List (List (Rope a)) -> b -> b
foldlHelperModuleLocal f queue res =
    case queue of
        [] ->
            res

        [] :: tail ->
            foldlHelperModuleLocal f tail res

        ((Rope.Leaf list) :: headTail) :: tail ->
            foldlHelperModuleLocal f (headTail :: tail) (List.foldl f res list)

        ((Rope.Node []) :: headTail) :: tail ->
            foldlHelperModuleLocal f (headTail :: tail) res

        ((Rope.Node childRopes) :: headTail) :: tail ->
            foldlHelperModuleLocal f (childRopes :: headTail :: tail) res


nested : (a -> b -> b) -> b -> Rope a -> b
nested f initialAcc rope =
    case rope of
        Rope.Leaf list ->
            List.foldl f initialAcc list

        Rope.Node [] ->
            initialAcc

        Rope.Node ropes ->
            List.foldl
                (\subRope acc ->
                    nested f acc subRope
                )
                initialAcc
                ropes
