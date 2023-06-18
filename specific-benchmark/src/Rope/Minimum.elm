module Rope.Minimum exposing (withFilterMap, withFoldl, withFoldr, withNestedFold)

import Rope exposing (Rope)
import Util exposing (listFilledFoldl1Map)


withFilterMap : Rope comparable -> Maybe comparable
withFilterMap rope =
    case rope of
        Rope.Leaf list ->
            List.minimum list

        Rope.Node ropes ->
            ropes
                |> List.filterMap withFilterMap
                |> List.minimum


withFoldr : Rope comparable -> Maybe comparable
withFoldr rope =
    Rope.foldr
        (\el acc ->
            Just
                (case acc of
                    Nothing ->
                        el

                    Just accContent ->
                        Basics.min accContent el
                )
        )
        Nothing
        rope


withFoldl : Rope comparable -> Maybe comparable
withFoldl rope =
    Rope.foldr
        (\el acc ->
            Just
                (case acc of
                    Nothing ->
                        el

                    Just accContent ->
                        Basics.min accContent el
                )
        )
        Nothing
        rope


withNestedFold : Rope comparable -> Maybe comparable
withNestedFold rope =
    case rope of
        Rope.Leaf list ->
            List.minimum list

        Rope.Node [] ->
            Nothing

        Rope.Node (headSubRope :: tailSubRopes) ->
            listFilledFoldl1Map withNestedFold
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
