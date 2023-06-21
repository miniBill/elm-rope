module Main exposing (main)

import Benchmark.Runner.Alternative
import Rope.Benchmark


main : Benchmark.Runner.Alternative.Program
main =
    Benchmark.Runner.Alternative.program Rope.Benchmark.all
