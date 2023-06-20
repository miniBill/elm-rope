module Rope.Reverse exposing (withFoldlToLeaf, withFoldrToLeaf)

import Rope.Local as Rope exposing (Rope)


withFoldlToLeaf : Rope a -> Rope a
withFoldlToLeaf rope =
    Rope.fromList <| Rope.foldl (::) [] rope


withFoldrToLeaf : (a -> b) -> Rope a -> Rope b
withFoldrToLeaf elementChange rope =
    Rope.foldr
        (\new soFar ->
            \end -> soFar (elementChange new :: end)
        )
        identity
        rope
        []
        |> Rope.fromList
