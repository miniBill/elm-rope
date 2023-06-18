module Rope.Length exposing (nested, withFoldl, withFoldr)

import Rope exposing (Rope)
import Util exposing (listSumMap)


withFoldl : Rope a -> Int
withFoldl rope =
    Rope.foldl (\_ l -> l + 1) 0 rope


withFoldr : Rope a -> Int
withFoldr rope =
    Rope.foldr (\_ l -> l + 1) 0 rope


nested : Rope a -> Int
nested rope =
    case rope of
        Rope.Leaf list ->
            List.length list

        Rope.Node ropes ->
            listSumMap nested ropes
