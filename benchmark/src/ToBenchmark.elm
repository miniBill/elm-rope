module ToBenchmark exposing (Function, Graph, config)

import Codec exposing (Codec)
import FastBenchmark.Config exposing (Config)
import FastBenchmark.Types exposing (Param)
import Rope


config : Config Graph Function
config =
    FastBenchmark.Config.init
        { graphTitle = graphTitle
        , graphCodec = graphCodec
        , functionToString = functionToString
        , functionCodec = functionCodec

        --
        , graphs = graphs
        , graphData =
            \_ ->
                { functions = functions
                , sizes = sizes
                }
        , runFunction = runFunction
        }
        |> FastBenchmark.Config.withTimeout timeout


type Graph
    = Simple
    | Deep


graphs : List Graph
graphs =
    [ --  Simple,
      Deep
    ]


graphTitle : Graph -> String
graphTitle graph =
    case graph of
        Simple ->
            "Two layers, (n * 10) elements"

        Deep ->
            "n layers, 2 elements"


graphCodec : Codec Graph
graphCodec =
    Codec.custom
        (\fsimple fdeep value ->
            case value of
                Simple ->
                    fsimple

                Deep ->
                    fdeep
        )
        |> Codec.variant0 "Simple" Simple
        |> Codec.variant0 "Deep" Deep
        |> Codec.buildCustom


type Function
    = List
    | Optimal
    | TCOd
    | Folded


functions : List Function
functions =
    [ List
    , Optimal
    , Folded
    , TCOd
    ]


functionToString : Function -> String
functionToString function =
    case function of
        List ->
            "List"

        Optimal ->
            "Optimal"

        Folded ->
            "Folded"

        TCOd ->
            "TCOd"


functionCodec : Codec Function
functionCodec =
    Codec.custom
        (\flist fOptimal ffolded ftcod value ->
            case value of
                List ->
                    flist

                Optimal ->
                    fOptimal

                Folded ->
                    ffolded

                TCOd ->
                    ftcod
        )
        |> Codec.variant0 "List" List
        |> Codec.variant0 "Optimal" Optimal
        |> Codec.variant0 "Folded" Folded
        |> Codec.variant0 "TCOd" TCOd
        |> Codec.buildCustom


sizes : List Int
sizes =
    List.range 10 17


runFunction : Param Graph Function -> () -> ()
runFunction param =
    case ( param.graph, param.function ) of
        ( Simple, List ) ->
            \_ -> ignore <| simpleList param.size

        ( Simple, Optimal ) ->
            \_ -> ignore <| simpleOptimal param.size

        ( Simple, Folded ) ->
            \_ -> ignore <| simpleFolded param.size

        ( Simple, TCOd ) ->
            \_ -> ignore <| simpleTcod param.size

        ( Deep, List ) ->
            \_ -> ignore <| deepList param.size

        ( Deep, Optimal ) ->
            \_ -> ignore <| deepOptimal param.size

        ( Deep, Folded ) ->
            \_ -> ignore <| deepFolded param.size

        ( Deep, TCOd ) ->
            \_ -> ignore <| deepTcod param.size


sizeToList : Int -> List Int
sizeToList size =
    List.range 1 (size * 10)


simpleList : Int -> List Int
simpleList size =
    let
        list =
            sizeToList size
    in
    list
        |> List.concatMap
            (\a ->
                list
                    |> List.concatMap
                        (\b ->
                            list
                                |> List.map (\c -> a * b * c)
                        )
            )


deepList : Int -> List Int
deepList size =
    if size <= 0 then
        [ 0, 1 ]

    else
        [ 0, 1 ]
            |> List.concatMap
                (\_ ->
                    deepList (size - 1)
                )


simpleOptimal : Int -> List Int
simpleOptimal size =
    let
        list =
            sizeToList size
    in
    list
        |> List.foldl
            (\a aacc ->
                list
                    |> List.foldl
                        (\b bacc ->
                            list
                                |> List.foldl
                                    (\c cacc -> a * b * c :: cacc)
                                    bacc
                        )
                        aacc
            )
            []


deepOptimal : Int -> List Int
deepOptimal =
    let
        go acc size =
            if size <= 0 then
                [ 0, 1 ] ++ acc

            else
                [ 0, 1 ]
                    |> List.foldl
                        (\_ iacc ->
                            go iacc (size - 1)
                        )
                        acc
    in
    go []


simpleFolded : Int -> List Int
simpleFolded size =
    let
        rope =
            Rope.fromList (sizeToList size)
    in
    rope
        |> Rope.concatMap
            (\a ->
                rope
                    |> Rope.concatMap
                        (\b ->
                            rope
                                |> Rope.map (\c -> a * b * c)
                        )
            )
        |> Rope.toList


deepFolded : Int -> List Int
deepFolded =
    let
        go size =
            if size <= 0 then
                Rope.fromList [ 0, 1 ]

            else
                Rope.fromList [ 0, 1 ]
                    |> Rope.concatMap
                        (\_ ->
                            go (size - 1)
                        )
    in
    Rope.toList << go


simpleTcod : Int -> List Int
simpleTcod size =
    let
        rope =
            Rope.fromList (sizeToList size)
    in
    rope
        |> Rope.concatMapTco
            (\a ->
                rope
                    |> Rope.concatMapTco
                        (\b ->
                            rope
                                |> Rope.map (\c -> a * b * c)
                        )
            )
        |> Rope.toList


deepTcod : Int -> List Int
deepTcod =
    let
        go size =
            if size <= 0 then
                Rope.fromList [ 0, 1 ]

            else
                Rope.fromList [ 0, 1 ]
                    |> Rope.concatMapTco
                        (\_ ->
                            go (size - 1)
                        )
    in
    Rope.toList << go


ignore : a -> ()
ignore _ =
    ()


timeout : Float
timeout =
    100
