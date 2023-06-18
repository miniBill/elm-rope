module Rope.ToList exposing (withFoldlReverse, withFoldr, withFoldrPreservingLastList)

import Rope exposing (Rope)


withFoldlReverse : Rope a -> List a
withFoldlReverse rope =
    Rope.foldl (::) [] rope |> List.reverse


withFoldr : Rope a -> List a
withFoldr rope =
    Rope.foldr (::) [] rope


{-| Just takes the last `Leaf list` a, making this a marginally more optimal `withFoldr`
-}
withFoldrPreservingLastList : Rope a -> List a
withFoldrPreservingLastList rope =
    case rope of
        Rope.Leaf list ->
            list

        Rope.Node ropes ->
            List.foldr
                (\subRope acc ->
                    { result =
                        if acc.isStart then
                            withFoldrPreservingLastList subRope

                        else
                            Rope.foldr (::) acc.result subRope
                    , isStart = False
                    }
                )
                { result = [], isStart = True }
                ropes
                |> .result
