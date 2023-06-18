module Rope.Sum exposing (nested, withFoldl, withFoldr)

import Rope exposing (Rope)
import Util exposing (listSumMap)


withFoldl : Rope number -> number
withFoldl rope =
    Rope.foldl (+) 0 rope


withFoldr : Rope number -> number
withFoldr rope =
    Rope.foldr (+) 0 rope


nested : Rope number -> number
nested rope =
    case rope of
        Rope.Leaf list ->
            List.sum list

        Rope.Node ropes ->
            listSumMap nested ropes
