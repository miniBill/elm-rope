module Rope.IsEmpty exposing (nested, withFoldl, withFoldr)

import Rope.Local as Rope exposing (Rope)
import Util exposing (listAll)


nested : Rope a -> Bool
nested rope =
    case rope of
        Rope.Leaf list ->
            List.isEmpty list

        Rope.Node ropes ->
            listAll nested ropes


withFoldl : Rope a -> Bool
withFoldl rope =
    case rope of
        Rope.Leaf list ->
            List.isEmpty list

        Rope.Node _ ->
            Rope.foldl (\_ _ -> False) True rope


withFoldr : Rope a -> Bool
withFoldr rope =
    case rope of
        Rope.Leaf list ->
            List.isEmpty list

        Rope.Node _ ->
            Rope.foldr (\_ _ -> False) True rope
